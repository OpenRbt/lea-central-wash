package service

import (
	"errors"
	"fmt"
	"hal/internal/app"
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/tarm/serial"
)

const uidAnswerRegex = "UID\\s([0-9A-F]*);"

const dispenserAnswer = "YF-S201;"

// Errors ...
var (
	ErrWrongAnswer = errors.New("wrong answer from the board")
)

var lastValue string = "0"

var flag int64 = 0

// PostError describes an error happened to a post
type PostError struct {
	PostNumber    int
	FailedCommand app.RelayConfig
}

// HardwareAccessLayer is the whole layer to communicate with boards
type HardwareAccessLayer struct {
	uidAnswer     *regexp.Regexp
	portsMu       sync.Mutex
	ports         map[string]*ports
	portRev2Board map[string]*Rev2Board
	dispencer     *Rev1DispencerBoard
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
}

type ports struct {
	osPath   string
	openPort *serial.Port
	toRemove bool
}

type Rev1DispencerBoard struct {
	osPath     string
	openPort   *serial.Port
	toRemove   bool
	errorCount int
}

// NewRev2Board is a constructor
func NewRev2Board(osPath string, openPort *serial.Port) *Rev2Board {
	return &Rev2Board{
		osPath:        osPath,
		openPort:      openPort,
		stationNumber: -1,
		toRemove:      false,
		Commands:      make(chan app.RelayConfig),
	}
}

// NewRevSensor is a constructor
func NewDispencerBoard(osPath string, openPort *serial.Port) *Rev1DispencerBoard {
	return &Rev1DispencerBoard{
		osPath:   osPath,
		openPort: openPort,
		toRemove: false,
	}
}

func NewPorts(osPath string, openPort *serial.Port) *ports {
	return &ports{
		osPath:   osPath,
		openPort: openPort,
		toRemove: false,
	}
}

// CollectAvailableSerialPorts reads /dev directory and find all ttyUSB* devices
func (h *HardwareAccessLayer) CollectAvailableSerialPorts() {
	h.uidAnswer, _ = regexp.Compile(uidAnswerRegex)
	files, err := ioutil.ReadDir("/dev")
	if err != nil {
		log.Fatal(err)
	}

	for _, f := range files {
		if strings.HasPrefix(f.Name(), "ttyUSB") {
			_, portExists := h.portByKey(f.Name())
			if !portExists {
				// port is not found in our dictionary
				err := h.checkAndAddPort(f.Name())
				if err == nil {
					fmt.Printf("New device is added [%s]\n", f.Name())
				} else {
					fmt.Printf("New device is not added [%s], err [%+v]\n", f.Name(), err)
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
	tick := time.Tick(800 * time.Millisecond)
	for {
		select {
		case <-tick:
			err := r.SendPing()
			if err != nil {
				r.errorCount++
				if r.errorCount >= 5 {
					r.toRemove = true
					return
				}
			} else {
				r.errorCount = 0
			}
		}
	}
}

func (h *HardwareAccessLayer) Volume() (int64, int64) {
	l, err := strconv.ParseInt(lastValue, 10, 10)
	if err != nil {
		fmt.Println("err: ", err)
	}
	fmt.Println("LasVolume = ", l)
	return l, flag
}

// Run command for Arduino
func (h *HardwareAccessLayer) Command(cmd int) error {
	r := h.dispencer
	go r.runCom(cmd)
	return nil
}

func (r *Rev1DispencerBoard) runCom(cmd int) error {
	lastValue = "0"
	buf := make([]byte, 32)
	cmdd := "S" + strconv.Itoa(cmd)
	exi := false
	flag = 1
	for i := 0; i < 10; i++ {
		_, err := r.openPort.Write([]byte(cmdd))
		if err != nil {
			fmt.Println("Error in command ", cmd)
			flag = 0
			return err
		} else {
			N, err := r.openPort.Read(buf)
			if err == nil {
				ans := string(buf[0 : N-2])
				if ans == "SOK;" {
					fmt.Println("Start command ", cmd)
					flag = 1
					exi = true
					break
				}
			}
		}
	}
	if exi {
		tick := time.Tick(200 * time.Millisecond)
		var countErr int
		for {
			select {
			case <-tick:
				N, err := r.openPort.Read(buf)
				if err == nil {
					ans := string(buf[0 : N-2])
					l, _ := strconv.ParseInt(lastValue, 10, 10)
					v, _ := strconv.ParseInt(ans[1:N-3], 10, 10)
					if v-l < 1 {
						countErr += 1
						if countErr >= 100 {
							_, err = r.openPort.Write([]byte("ERR;"))
							N, err = r.openPort.Read(buf)
							ans = string(buf[0 : N-2])
							if ans == "FOK;" {
								flag = 0
								fmt.Println("The non-freezing is over")
								return nil
							}
						}
					}
					fmt.Println("Answer: ", ans)
					if ans[0:N-2] != "OK-PING;" {
						lastValue = ans[1 : N-3]
					}
					fmt.Println("Answer Arduino", lastValue)
					if ans[0] == 'F' {
						fmt.Println("Finish command ", cmd, " Successfully!")
						flag = 0
						countErr = 0
						_, err = r.openPort.Write([]byte("FOK;"))
						if err != nil {
							fmt.Println("Error in command FOK")
							return err
						}
						return nil
					}
				} else {
					flag = 0
					fmt.Println(err)
					fmt.Println("Error in command ", cmd)
					return err
				}
			}
		}
	}
	flag = 0
	fmt.Println("Arduino is not responding ")
	return nil
}

// Run just runs a goroutine which controls a device
func (r *Rev2Board) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev2Board) workingLoop() {
	tick := time.Tick(800 * time.Millisecond)
	var cmd app.RelayConfig
	for {
		select {
		case <-tick:
			stationNumber, err := r.SendPing()
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
				err = r.runCommand(cmd)
			}
		}
	}
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
		return err
	}

	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
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
	if h.dispencer.osPath == key {
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
		sensor := NewDispencerBoard(key, s)
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
	board := NewRev2Board(key, s)
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
	for key := range h.ports {
		if h.portRev2Board[key].stationNumber == int(wantedPosition) {
			return h.portRev2Board[key], nil
		}
	}
	fmt.Printf("board #%d is not found on dictionary\n", wantedPosition)
	return nil, app.ErrNotFound
}

// Start just starts everything
func (h *HardwareAccessLayer) Start() {
	go h.workingLoop()
}

func (h *HardwareAccessLayer) workingLoop() ([]app.ControlBoard, error) {
	for {
		h.CollectAvailableSerialPorts()
		t := true
		h.portsMu.Lock()
		for t {
			t = false
			for key := range h.ports {
				if h.ports[key].toRemove {
					delete(h.ports, key)
					t = true
					break
				}
			}
		}
		h.portsMu.Unlock()
		time.Sleep(time.Second) // Let's do maintenance just once per second
	}
}

// NewHardwareAccessLayer is just a constructor
func NewHardwareAccessLayer() (app.HardwareAccessLayer, error) {
	res := &HardwareAccessLayer{
		ports:         make(map[string]*ports),
		portRev2Board: make(map[string]*Rev2Board),
	}
	return res, nil
}

// RunProgram
func (h *HardwareAccessLayer) RunProgram(id int32, cfg app.RelayConfig) (err error) {
	log.Printf("Program config: stationID=%d, motor speed=%d", id, cfg.MotorSpeedPercent)
	board, err := h.ControlBoard(id)
	if err != nil {
		return err
	}
	board.RunConfig(cfg)
	return nil
}

// HardwareDebugAccessLayer is the whole layer to communicate with boards
type HardwareDebugAccessLayer struct {
	portsMu sync.Mutex
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
func (h HardwareDebugAccessLayer) Run2Programs(id int32, secondID int32, cfg app.RelayConfig) (err error) {
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
	for key := range h.portsRev2Board {
		if h.portsRev2Board[key].stationNumber == int(wantedPosition) {
			return h.portsRev2Board[key], nil
		}
	}
	// defer h.portsMu.Unlock()
	return nil, app.ErrNotFound
}

// RunConfig just runs a config
func (r *Rev2DebugBoard) RunConfig(config app.RelayConfig) {
	log.Printf("Running at motor speed=%d, timeout=%d", config.MotorSpeedPercent, config.TimeoutSec)
	for i := 0; i < len(config.Timings); i++ {
		log.Printf("Relay ID=%d, timeon=%d, timeoff=%d", config.Timings[i].ID, config.Timings[i].TimeOn, config.Timings[i].TimeOff)
	}
}

// MyPosition returns current post position
func (r *Rev2DebugBoard) MyPosition() (int, error) {
	return r.stationNumber, nil
}

// StopAll just stops all relays
func (r *Rev2DebugBoard) StopAll() error {
	// Please add here RunCommand with all zero relays
	return errors.New("not implemented")
}
