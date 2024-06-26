package service

import (
	"context"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485"

	"github.com/tarm/serial"
)

const uidAnswerRegex = "UID\\s([0-9A-F@]*);"

const dispenserAnswer = "YF-S201;"

// Errors ...
var (
	ErrWrongAnswer = errors.New("wrong answer from the board")
)

// PostError describes an error happened to a post
type PostError struct {
	PostNumber    int
	FailedCommand app.RelayConfig
}

// HardwareAccessLayer is the whole layer to communicate with boards
type HardwareAccessLayer struct {
	uidAnswer     *regexp.Regexp
	ports         map[string]*ports
	portRev2Board map[string]*Rev2Board
	portsMu       sync.RWMutex // used for both ports and portRev2Board
	dispencer     *Rev1DispencerBoard
	motorManager  rs485.MotorManager
	hwMetrics     app.HardwareMetrics
	Errors        chan PostError
}

// Rev2Board describes Revision 2 openrbt.com board
type Rev2Board struct {
	osPath        string
	openPort      *serial.Port
	toRemove      bool
	stationNumber int
	errorCount    int
	Commands      chan app.RelayConfig
	hwMetrics     app.HardwareMetrics
}

type ports struct {
	osPath   string
	openPort *serial.Port
	toRemove bool
}

type Rev1DispencerBoard struct {
	resourcesMu           sync.Mutex
	osPath                string
	openPort              *serial.Port
	toRemove              bool
	errorCount            int
	lastVolume            int64
	allowedPing           bool
	stopDispenser         bool
	ErrorCommandDispenser error
	commandStopRev2Board  app.RelayConfig
	commandStartRev2Board app.RelayConfig
	timeoutSec            int
	runOnRev2Board        func(int32, app.RelayConfig) error
	hwMetrics             app.HardwareMetrics
}

// NewRev2Board is a constructor, using USB to communicate
func NewRev2Board(osPath string, openPort *serial.Port, hwMetrics app.HardwareMetrics) *Rev2Board {
	return &Rev2Board{
		osPath:        osPath,
		openPort:      openPort,
		stationNumber: -1,
		toRemove:      false,
		Commands:      make(chan app.RelayConfig),
		hwMetrics:     hwMetrics,
	}
}

// NewDispencerBoard is a constructor
func NewDispencerBoard(osPath string, openPort *serial.Port, runOnRev2Board func(int32, app.RelayConfig) error, hwMetrics app.HardwareMetrics) *Rev1DispencerBoard {
	return &Rev1DispencerBoard{
		osPath:                osPath,
		openPort:              openPort,
		toRemove:              false,
		allowedPing:           true,
		stopDispenser:         false,
		ErrorCommandDispenser: nil,
		timeoutSec:            3,
		runOnRev2Board:        runOnRev2Board,
		hwMetrics:             hwMetrics,
	}
}

func NewPorts(osPath string, openPort *serial.Port) *ports {
	return &ports{
		osPath:   osPath,
		openPort: openPort,
		toRemove: false,
	}
}

func (r *Rev1DispencerBoard) SetStopDispenser(t bool) {
	r.resourcesMu.Lock()
	r.stopDispenser = t
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetStopDispenser() bool {
	r.resourcesMu.Lock()
	t := r.stopDispenser
	r.resourcesMu.Unlock()
	return t
}

func (r *Rev1DispencerBoard) SetLastVolume(volume int64) {
	r.resourcesMu.Lock()
	r.lastVolume = volume
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetLastVolume() int64 {
	r.resourcesMu.Lock()
	volume := r.lastVolume
	r.resourcesMu.Unlock()
	return volume
}

func (r *Rev1DispencerBoard) SetAllowedPing(t bool) {
	r.resourcesMu.Lock()
	r.allowedPing = t
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetAllowedPing() bool {
	r.resourcesMu.Lock()
	t := r.allowedPing
	r.resourcesMu.Unlock()
	return t
}

func (r *Rev1DispencerBoard) SetErrComandDispenser(err error) {
	r.resourcesMu.Lock()
	r.ErrorCommandDispenser = err
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetErrComandDispenser() error {
	r.resourcesMu.Lock()
	err := r.ErrorCommandDispenser
	r.resourcesMu.Unlock()
	return err
}

func (r *Rev1DispencerBoard) RunCommandStartRev2Board() error {
	r.resourcesMu.Lock()
	relay := r.commandStartRev2Board
	r.resourcesMu.Unlock()
	return r.runOnRev2Board(1, relay)
}

func (r *Rev1DispencerBoard) RunCommandStopRev2Board() error {
	r.resourcesMu.Lock()
	relay := r.commandStopRev2Board
	r.resourcesMu.Unlock()
	return r.runOnRev2Board(1, relay)
}

// CollectAvailableSerialPorts reads /dev directory and find all ttyUSB* and ttyACM* devices
func (h *HardwareAccessLayer) CollectAvailableSerialPorts() {
	h.uidAnswer, _ = regexp.Compile(uidAnswerRegex)
	files, err := ioutil.ReadDir("/dev")
	if err != nil {
		log.Fatal(err)
	}

	for _, f := range files {
		if strings.HasPrefix(f.Name(), "ttyUSB") || strings.HasPrefix(f.Name(), "ttyACM") {
			_, portExists := h.portByKey(f.Name())
			if !portExists {
				fmt.Printf("trying to check as RS485 %s \n", f.Name())
				err = h.motorManager.TryAddDevice(f.Name())
				if err == nil {
					h.addPort(f.Name(), &ports{osPath: f.Name()})
					fmt.Printf("added as rs485 controller [%s]\n", f.Name())
					continue
				}
				// port is not found in our dictionary
				err = h.checkAndAddPort(f.Name())
				if err == nil {
					fmt.Printf("New board is added [%s]\n", f.Name())
				} else {
					fmt.Printf("New board is not added [%s], err [%+v]\n", f.Name(), err)
				}
			}
		}
	}
}

func (r *Rev1DispencerBoard) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev1DispencerBoard) workingLoop() {
	duration := 800 * time.Millisecond
	timer := time.NewTimer(duration)
	for {
		<-timer.C
		timer.Stop()
		if r.GetAllowedPing() {
			err := r.SendPing()
			if err != nil {
				r.resourcesMu.Lock()
				r.errorCount++
				errCount := r.errorCount
				r.resourcesMu.Unlock()
				if errCount >= 5 {
					fmt.Println("Delete Rev1DispenserBoard")
					r.resourcesMu.Lock()
					r.toRemove = true
					r.resourcesMu.Unlock()
					return
				}
			} else {
				r.resourcesMu.Lock()
				r.errorCount = 0
				r.resourcesMu.Unlock()
			}
		} else {
			r.SetAllowedPing(true)
		}
		timer.Reset(duration)
	}
}

func (h *HardwareAccessLayer) GetLevel() int {
	r := h.dispencer
	if r == nil {
		fmt.Println("Dispenser is nil")
		return -1
	}
	return r.getLevel()
}

func (r *Rev1DispencerBoard) getLevel() int {
	cmd := "L;"
	_, err := r.openPort.Write([]byte(cmd))
	if err != nil {
		fmt.Println("Error in command", cmd)
		return -1
	}
	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
		fmt.Println("Error in read answer")
		return -1
	}
	ans := string(buf[0 : N-2])
	n, err := strconv.Atoi(ans)
	if err != nil {
		fmt.Println("Error in convert string in int")
		return -1
	}
	return n
}

func (h *HardwareAccessLayer) Volume() app.DispenserStatus {
	r := h.dispencer
	if r == nil {
		return app.DispenserStatus{
			Milliliters:           0,
			ErrorCommandDispenser: app.ErrNotFoundDispenser,
		}
	}
	return r.volume()
}

func (r *Rev1DispencerBoard) volume() app.DispenserStatus {
	return app.DispenserStatus{
		Milliliters:           int64(r.GetLastVolume()),
		ErrorCommandDispenser: r.GetErrComandDispenser(),
	}
}

// Run command for Arduino
func (h *HardwareAccessLayer) MeasureVolumeMilliliters(measureVolume int, stationID int32, startCfg app.RelayConfig, stopCfg app.RelayConfig) error {
	r := h.dispencer
	if r == nil {
		return app.ErrNotFoundDispenser
	}

	board, _ := h.ControlBoard(stationID)
	if board == nil {
		return app.ErrNotFoundBoard
	}

	stCfg := app.RelayConfig{
		MotorSpeedPercent: startCfg.MotorSpeedPercent,
		TimeoutSec:        int32(r.timeoutSec),
		Timings:           startCfg.Timings,
	}

	r.resourcesMu.Lock()
	r.commandStopRev2Board = stopCfg
	r.commandStartRev2Board = stCfg
	r.resourcesMu.Unlock()
	r.SetStopDispenser(false)
	go r.measureVolumeMilliliters(measureVolume)
	return nil
}

func (h *HardwareAccessLayer) DeviceInfo() string {

	// rs485 motors
	motorManagerInfo := h.motorManager.DeviceInfo()

	// control boards
	resBoard := make(map[string]int32)
	for i := int32(1); i < app.MAX_ALLOWED_DEVICES; i++ {
		brd, err := h.ControlBoard(i)
		if err == nil {
			resBoard[brd.Port()] = i
		}
	}

	// ports in use
	resPorts := make(map[string]int32)
	h.portsMu.RLock()
	for key := range h.ports {
		resPorts[key] = 1
	}
	h.portsMu.RUnlock()

	return fmt.Sprintf("RS485:\n%+v\n\nBoards:\n%+v\n\nPorts in use:\n%+v\n", motorManagerInfo, resBoard, resPorts)
}

func (h *HardwareAccessLayer) DispenserStop(cfg app.RelayConfig) error {
	r := h.dispencer
	if r == nil {
		return app.ErrNotFoundDispenser
	}
	r.resourcesMu.Lock()
	r.commandStopRev2Board = cfg
	r.resourcesMu.Unlock()
	return r.dispenserStop()
}

func (r *Rev1DispencerBoard) dispenserStop() error {
	r.SetStopDispenser(true)
	return nil
}

func (r *Rev1DispencerBoard) reconnect() error {
	r.hwMetrics.Rev1DispencerReconnectCounter.Inc("1")
	r.openPort.Close()
	time.Sleep(20 * time.Millisecond)
	c := &serial.Config{Name: "/dev/" + r.osPath, Baud: 38400, ReadTimeout: time.Millisecond * 100}
	s, err := serial.OpenPort(c)
	if err == nil {
		r.openPort = s
		fmt.Printf("reconnect OK\n")
	} else {
		fmt.Printf("reconnect err: %s\n", err)
	}
	return err
}

func (r *Rev1DispencerBoard) measureVolumeMilliliters(measureVolume int) error {
	r.SetAllowedPing(false)
	r.SetLastVolume(0)
	r.SetErrComandDispenser(nil)
	countErrRead := 0
	stopDispenser := 0
	startFluid := 20
	buf := make([]byte, 32)
	cmdd := "S" + strconv.Itoa(measureVolume) + ";"
	exi := false
	fmt.Println("In measure")
	for i := 0; i < 10; i++ {
		_, err := r.openPort.Write([]byte(cmdd))
		if err != nil {
			fmt.Println("Error in command ", measureVolume)
			r.SetErrComandDispenser(err)
		} else {
			N, err := r.openPort.Read(buf)
			if err == nil && N > 1 {
				ans := string(buf[0 : N-2])
				if ans == "SOK;" {
					fmt.Println("Start command ", measureVolume)
					r.SetErrComandDispenser(nil)
					exi = true
					break
				}
			}
		}
	}
	if exi {
		tick := time.Tick(200 * time.Millisecond)
		var countErr int = 0
		for {
			select {
			case <-tick:
				if startFluid > 5 {
					err := r.RunCommandStartRev2Board()
					if err != nil {
						fmt.Printf("error RunCommandStartRev2Board: %s\n", err)
					} else {
						startFluid = 0
					}
				}
				startFluid++
				r.SetAllowedPing(false)
				if r.GetStopDispenser() {
					if stopDispenser > 0 {
						_, err := r.openPort.Write([]byte("FOK;"))
						if err != nil {
							fmt.Println("Error in command FOK")
							r.SetErrComandDispenser(app.ErrInCommandFok)
							return app.ErrInCommandFok
						}
						return nil
					}
					err := r.RunCommandStopRev2Board()
					if err != nil {
						fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
					} else {
						stopDispenser++
					}
				}
				N, err := r.openPort.Read(buf)
				if err == nil && N > 2 {
					countErrRead = 0
					ans := string(buf[0 : N-2])
					v, err := strconv.ParseInt(ans[1:N-3], 10, 64)
					if err != nil {
						v = r.GetLastVolume()
						fmt.Println(err)
					}
					fmt.Printf("Current volume: %d\n", v)
					if v-r.GetLastVolume() < 1 {
						countErr += 1
						if countErr >= 50 {
							err := r.RunCommandStopRev2Board()
							if err != nil {
								fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
							}
							_, err = r.openPort.Write([]byte("ERR;"))
							N, err = r.openPort.Read(buf)
							ans = string(buf[0 : N-2])
							if ans == "FOK;" {
								r.SetErrComandDispenser(app.ErrNonFreezing)
								fmt.Println("The non-freezing is over")
								return app.ErrNonFreezing
							}
						}
					} else {
						countErr = 0
						r.SetLastVolume(v)
					}
					if ans[0] == 'F' || v >= int64(measureVolume) {
						fmt.Println("Finish command ", measureVolume, " Successfully!")
						err := r.RunCommandStopRev2Board()
						if err != nil {
							fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
						}
						countErr = 0
						r.SetLastVolume(v)
						_, err = r.openPort.Write([]byte("FOK;"))
						if err != nil {
							r.SetErrComandDispenser(app.ErrInCommandFok)
							fmt.Println("Error in command FOK")
							return app.ErrInCommandFok
						}
						return nil
					}
				} else {
					countErrRead += 1
					fmt.Println("Error read answer dispenser")
					if countErrRead > 2 {
						r.reconnect()
					}
					if countErrRead > 5 {
						err := r.RunCommandStopRev2Board()
						if err != nil {
							fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
						} else {
							stopDispenser++
						}
						_, _ = r.openPort.Write([]byte("FOK;"))
						r.SetErrComandDispenser(app.ErrReadAnswerDispenser)
						fmt.Println("Error in command ", measureVolume)
						return err
					}
				}
			}
		}
	}
	r.SetErrComandDispenser(app.ErrDispenserNotRespond)
	fmt.Println("Dispenser is not responding")
	err := r.RunCommandStopRev2Board()
	if err != nil {
		fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
	}
	return app.ErrDispenserNotRespond
}

// Run just runs a goroutine which controls a device
func (r *Rev2Board) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev2Board) workingLoop() {
	duration := 800 * time.Millisecond
	timer := time.NewTimer(duration)
	var cmd app.RelayConfig
	for {
		select {
		case <-timer.C:
			timer.Stop()
			stationNumber, err := r.SendPing()
			timer.Reset(duration)
			if err != nil {
				r.errorCount++
				if r.errorCount >= 5 {
					r.toRemove = true
					return
				}
			} else {
				r.stationNumber = stationNumber
				r.errorCount = 0
			}
		case cmd = <-r.Commands:
			err := r.runCommand(cmd)
			for err != nil {
				r.errorCount++
				if r.errorCount >= 5 {
					r.toRemove = true
					return
				}
				if r.errorCount > 1 {
					r.reconnect()
				}
				err = r.runCommand(cmd)
			}
		}
	}
}

func (r *Rev2Board) reconnect() error {
	r.hwMetrics.Rev2BoardReconnectCounter.Inc(strconv.Itoa(r.stationNumber))
	r.openPort.Close()
	time.Sleep(20 * time.Millisecond)
	c := &serial.Config{Name: "/dev/" + r.osPath, Baud: 38400, ReadTimeout: time.Millisecond * 100}
	s, err := serial.OpenPort(c)
	if err == nil {
		r.openPort = s
		fmt.Printf("reconnect OK\n")
	} else {
		fmt.Printf("reconnect err: %s\n", err)
	}
	return err
}

func (r *Rev2Board) runCommand(cmd app.RelayConfig) error {
	var cmdBuf strings.Builder
	cmdBuf.Grow(192)
	finalRelays := make(map[int]app.Relay, 12)
	for _, relayItem := range cmd.Timings {
		finalRelays[relayItem.ID] = relayItem
	}
	cmdBuf.WriteString("RUN A-|")
	if cmd.TimeoutSec > 0 {
		cmdBuf.WriteString("T")
		cmdBuf.WriteString(strconv.Itoa(int(cmd.TimeoutSec)))
		cmdBuf.WriteString("|")
	}
	if cmd.MotorSpeedPercent >= 0 {
		cmdBuf.WriteString("M")
		cmdBuf.WriteString(strconv.Itoa(int(cmd.MotorSpeedPercent / 2)))
		cmdBuf.WriteString("|")
	}
	// 'A-' means to turn off not mentioned relays
	for i := 1; i <= 11; i++ {
		if relay, ok := finalRelays[i]; ok {
			if relay.TimeOn > 0 {
				cmdBuf.WriteString(strconv.Itoa(i))
				if relay.TimeOff > 0 {
					cmdBuf.WriteString("/")
					cmdBuf.WriteString(strconv.Itoa(relay.TimeOn))
					cmdBuf.WriteString("/")
					cmdBuf.WriteString(strconv.Itoa(relay.TimeOff))
				}
				cmdBuf.WriteString("|")
			}
		} else {
			// no relay in program
			// let's not write anything while we have 'A-' option turned on
		}
	}
	cmdBuf.WriteString(";")
	finalCmd := cmdBuf.String()
	fmt.Println("-----------------")
	fmt.Println(finalCmd)
	_, err := r.openPort.Write([]byte(finalCmd))
	if err != nil {
		fmt.Println(err)
		return err
	}

	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
		fmt.Println(err)
		return err
	}

	if N < 2 {
		return ErrWrongAnswer
	}

	if string(buf[0:2]) != "OK" {
		return ErrWrongAnswer
	}

	fmt.Println("Got OK from running board!")

	return nil
}

// MyPosition returns current post position
func (r *Rev2Board) MyPosition() (int, error) {
	return r.stationNumber, nil
}

// StopAll just stops all relays
func (r *Rev2Board) StopAll() error {
	// Please add here RunCommand with all zero relays
	return errors.New("not implemented")
}

func (r *Rev1DispencerBoard) SendPing() error {
	_, err := r.openPort.Write([]byte("PING;"))
	if err != nil {
		return err
	}

	buf := make([]byte, 32)
	_, err = r.openPort.Read(buf)
	if err != nil {
		return err
	}
	st := string(buf[0:8])
	if st != "OK-PING;" {
		return err
	}
	fmt.Println("Ping-OK")
	return nil
}

// SendPing just pings the station periodically
func (r *Rev2Board) SendPing() (int, error) {
	_, err := r.openPort.Write([]byte("PING;"))
	if err != nil {
		return 0, err
	}

	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
		return 0, err
	}
	if N < 2 {
		return 0, ErrWrongAnswer
	}
	answer, err := strconv.ParseInt(string(buf[0:2]), 10, 0)
	if err != nil {
		return 0, err
	}
	fmt.Printf("ping ok, post=%d\n", answer)
	return int(answer), nil
}

func (h *HardwareAccessLayer) portByKey(key string) (*ports, bool) {
	h.portsMu.Lock()
	el, found := h.ports[key]
	h.portsMu.Unlock()
	if !found {
		return nil, false
	}
	return el, true
}

func (h *HardwareAccessLayer) deletePort(key string) {
	h.portsMu.Lock()
	delete(h.ports, key)
	if h.dispencer != nil && h.dispencer.osPath == key {
		h.dispencer = nil
	} else {
		delete(h.portRev2Board, key)
	}
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) addPort(key string, port *ports) {
	h.portsMu.Lock()
	h.ports[key] = port
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) addRev2Board(key string, board *Rev2Board) {
	h.portsMu.Lock()
	h.portRev2Board[key] = board
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) addDispencerBoard(key string, sensor *Rev1DispencerBoard) {
	h.portsMu.Lock()
	h.dispencer = sensor
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) checkAndAddPort(key string) error {
	c := &serial.Config{Name: "/dev/" + key, Baud: 38400, ReadTimeout: time.Millisecond * 100}
	s, err := serial.OpenPort(c)
	if err != nil {
		return err
	}

	_, err = s.Write([]byte(";"))
	if err != nil {
		return err
	}
	err = s.Flush()
	if err != nil {
		return err
	}

	_, err = s.Write([]byte("UID;"))
	if err != nil {
		return err
	}

	buf := make([]byte, 128)
	N, err := s.Read(buf)
	if err != nil {
		s.Close()
		return err
	}
	if N < 2 {
		s.Close()
		return nil
	}
	ans := string(buf[0:N])
	fmt.Printf("answer is [%s]\n", ans)
	if dispenserAnswer == ans {
		sensor := NewDispencerBoard(key, s, h.runOnRev2Board, h.hwMetrics)
		port := NewPorts(key, s)
		h.addDispencerBoard(key, sensor)
		h.addPort(key, port)
		return sensor.Run()
	}
	foundStrings := h.uidAnswer.FindStringSubmatch(ans)
	if len(foundStrings) < 2 {
		s.Close()
		return nil
	}

	fmt.Printf("uid is [%s]\n", foundStrings[1])
	board := NewRev2Board(key, s, h.hwMetrics)
	port := NewPorts(key, s)
	h.addRev2Board(key, board)
	h.addPort(key, port)
	return board.Run()
}

// RunConfig just runs a config
func (r *Rev2Board) RunConfig(config app.RelayConfig) {
	r.Commands <- config
}

// ControlBoard returns required control board by its key
func (h *HardwareAccessLayer) ControlBoard(wantedPosition int32) (app.ControlBoard, error) {
	h.portsMu.Lock()
	defer h.portsMu.Unlock()
	for key := range h.portRev2Board {
		if h.portRev2Board[key].stationNumber == int(wantedPosition) {
			return h.portRev2Board[key], nil
		}
	}
	// fmt.Printf("board #%d is not found on dictionary\n", wantedPosition)
	return nil, app.ErrNotFound
}

// Start just starts everything
func (h *HardwareAccessLayer) Start() {
	go h.workingLoop()
	h.motorManager.Run()
}

func (h *HardwareAccessLayer) workingLoop() ([]app.ControlBoard, error) {
	for {
		h.CollectAvailableSerialPorts()
		t := true
		h.portsMu.Lock()
		for t {
			t = false
			for key := range h.portRev2Board {
				if h.portRev2Board[key].toRemove {
					h.portRev2Board[key].openPort.Close()
					delete(h.ports, h.portRev2Board[key].osPath)
					delete(h.portRev2Board, key)
					t = true
					break
				}
			}
		}
		if h.dispencer != nil {
			if h.dispencer.toRemove {
				h.dispencer.openPort.Close()
				delete(h.ports, h.dispencer.osPath)
				h.dispencer = nil
			}
		}
		h.portsMu.Unlock()
		time.Sleep(time.Second) // Let's do maintenance just once per second
	}
}

// NewHardwareAccessLayer is just a constructor
func NewHardwareAccessLayer(newMetrics app.HardwareMetrics) (app.HardwareAccessLayer, error) {
	res := &HardwareAccessLayer{
		ports:         make(map[string]*ports),
		portRev2Board: make(map[string]*Rev2Board),
		hwMetrics:     newMetrics,
	}
	model := rs485.FreqGenModelESQ500
	if strings.ToLower(os.Getenv("ESQ_MODEL")) == "ae200h" {
		model = rs485.FreqGenModelAE200H
	}
	res.motorManager = *rs485.NewMotorManager(context.Background(), newMetrics, res, time.Second*10, model)
	return res, nil
}

func (h *HardwareAccessLayer) FreePort(portName string) {
	h.deletePort(portName)
}

// RunProgram
func (h *HardwareAccessLayer) RunProgram(id int32, cfg app.RelayConfig) (err error) {
	// log.Printf("Program config: stationID=%d, motor speed=%d", id, cfg.MotorSpeedPercent)

	errBoard := h.runOnRev2Board(id, cfg)
	errRS := h.runOnRS(id, cfg)
	if errBoard == nil || errRS == nil {
		return nil
	}

	err1 := fmt.Errorf("can`t run program, %w", errBoard)
	errAll := fmt.Errorf("%v, %w", err1, errRS)

	return errAll
}

func (h *HardwareAccessLayer) runOnRS(id int32, cfg app.RelayConfig) error {
	err1 := h.motorManager.StartMotor(uint8(id))
	err2 := h.motorManager.SetSpeedPercent(uint8(id), int16(cfg.MotorSpeedPercent))
	if err1 != nil {
		return err1
	}
	if err2 != nil {
		return err2
	}
	return nil
}

func (h *HardwareAccessLayer) runOnRev2Board(id int32, cfg app.RelayConfig) error {
	board, err := h.ControlBoard(id)
	if err != nil {
		return err
	}
	board.RunConfig(cfg)
	return nil
}

// HardwareDebugAccessLayer is the whole layer to communicate with boards
type HardwareDebugAccessLayer struct {
	portsMu sync.RWMutex
	// ports   map[string]*ports
	portsRev2Board map[string]*Rev2DebugBoard
}

// Rev2DebugBoard describes Revision 2 openrbt.com board
type Rev2DebugBoard struct {
	stationNumber int
	errorCount    int
	Commands      chan app.RelayConfig
}

// NewRev2DebugBoard is a constructor
func NewRev2DebugBoard(stationNumber int) *Rev2DebugBoard {
	return &Rev2DebugBoard{
		stationNumber: stationNumber,
		errorCount:    0,
		Commands:      make(chan app.RelayConfig),
	}
}

// NewHardwareDebugAccessLayer is just a constructor
func NewHardwareDebugAccessLayer() (app.HardwareAccessLayer, error) {
	res := &HardwareAccessLayer{
		// ports:         make(map[string]*ports),
		portRev2Board: make(map[string]*Rev2Board),
	}
	return res, nil
}

// Start just starts everything
func (h *HardwareDebugAccessLayer) Start() {
	h.portsMu.Lock()
	h.portsRev2Board["testboard1"] = NewRev2DebugBoard(1)
	h.portsRev2Board["testboard2"] = NewRev2DebugBoard(2)
	h.portsRev2Board["testboard3"] = NewRev2DebugBoard(3)
	h.portsRev2Board["testboard4"] = NewRev2DebugBoard(4)
	h.portsRev2Board["testboard5"] = NewRev2DebugBoard(5)
	h.portsRev2Board["testboard6"] = NewRev2DebugBoard(6)
	h.portsMu.Unlock()
}

// RunProgram
func (h *HardwareDebugAccessLayer) RunProgram(id int32, cfg app.RelayConfig) (err error) {
	log.Printf("Program config: stationID=%d, motor speed=%d", id, cfg.MotorSpeedPercent)
	board, err := h.ControlBoard(id)
	if err != nil {
		return err
	}
	go board.RunConfig(cfg)
	return nil
}

// Run2Programs
func (h *HardwareDebugAccessLayer) Run2Programs(id int32, secondID int32, cfg app.RelayConfig) (err error) {
	err = h.RunProgram(id, cfg)
	if err != nil {
		return err
	}

	err = h.RunProgram(secondID, cfg)
	if err != nil {
		return err
	}

	return nil
}

// ControlBoard returns required control board by its key
func (h *HardwareDebugAccessLayer) ControlBoard(wantedPosition int32) (app.ControlBoard, error) {
	// h.portsMu.Lock()
	h.portsMu.RLock()
	defer h.portsMu.RUnlock()
	for key := range h.portsRev2Board {
		if h.portsRev2Board[key].stationNumber == int(wantedPosition) {
			return h.portsRev2Board[key], nil
		}
	}
	return nil, app.ErrNotFound
}

// RunConfig just runs a config
func (r *Rev2DebugBoard) RunConfig(config app.RelayConfig) {
	log.Printf("Running at motor speed=%d, timeout=%d", config.MotorSpeedPercent, config.TimeoutSec)
	for i := 0; i < len(config.Timings); i++ {
		log.Printf("Relay ID=%d, timeon=%d, timeoff=%d", config.Timings[i].ID, config.Timings[i].TimeOn, config.Timings[i].TimeOff)
	}
}
func (r *Rev2DebugBoard) Port() string {
	return "debugUSB0"
}

// MyPosition returns current post position
func (r *Rev2DebugBoard) MyPosition() (int, error) {
	return r.stationNumber, nil
}

func (r *Rev2Board) Port() string {
	return r.osPath
}

// StopAll just stops all relays
func (r *Rev2DebugBoard) StopAll() error {
	// Please add here RunCommand with all zero relays
	return errors.New("not implemented")
}
