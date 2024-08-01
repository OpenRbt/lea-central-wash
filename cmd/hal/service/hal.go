package service

import (
	"context"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
	"strings"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485"

	"github.com/tarm/serial"
)

const uidAnswerRegex = "UID\\s([0-9A-F@]*);"

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
	uidAnswer       *regexp.Regexp
	ports           map[string]*ports
	portRev2Board   map[string]*Rev2Board
	portRev2BoardRS []*Rev2BoardRS
	portsMu         sync.RWMutex // used for both ports and portRev2Board
	dispencer       *Rev1DispencerBoard
	motorManager    rs485.MotorManager
	hwMetrics       app.HardwareMetrics
	Errors          chan PostError
}

type ports struct {
	osPath   string
	openPort *serial.Port
	toRemove bool
}

func NewPorts(osPath string, openPort *serial.Port) *ports {
	return &ports{
		osPath:   osPath,
		openPort: openPort,
		toRemove: false,
	}
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

				err = h.checkAndAddPortRS(f.Name())
				if err == nil {
					fmt.Printf("New board rs is added [%s]\n", f.Name())
					continue
				} else {
					fmt.Printf("New board rs is not added [%s], err [%+v]\n", f.Name(), err)
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

func (h *HardwareAccessLayer) GetLevel() int {
	r := h.dispencer
	if r == nil {
		fmt.Println("Dispenser is nil")
		return -1
	}
	return r.getLevel()
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
			for i := range h.portRev2BoardRS {
				if h.portRev2BoardRS[i].toRemove {
					h.portRev2BoardRS[i].openPort.Close()
					delete(h.ports, h.portRev2BoardRS[i].osPath)
					h.portRev2BoardRS = append(h.portRev2BoardRS[:i], h.portRev2BoardRS[i+1:]...)
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
	var model rs485.FreqGenModel
	switch strings.ToLower(os.Getenv("ESQ_MODEL")) {
	case "ae200h":
		model = rs485.FreqGenModelAE200H
	case "esq770":
		model = rs485.FreqGenModelESQ770
	default:
		model = rs485.FreqGenModelESQ500
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
	cfg.StationID = id
	errBoard := h.runOnRev2Board(id, cfg)
	errBoardRS := h.runOnRev2BoardRS(cfg)
	errRS := h.runOnRS(id, cfg)
	if errBoard == nil || errRS == nil || errBoardRS == nil {
		return nil
	}

	err1 := fmt.Errorf("can`t run program, %w", errBoard)
	errAll := fmt.Errorf("%v, %w", err1, fmt.Errorf("%v, %w", errRS, errBoardRS))

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

func (h *HardwareAccessLayer) checkAndAddPortRS(key string) error {
	c := &serial.Config{Name: "/dev/" + key, Baud: 9600, ReadTimeout: time.Millisecond * 50}
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
	for i := 1; i <= app.MAX_ALLOWED_DEVICES; i++ {
		_, err = s.Write([]byte(fmt.Sprintf("S%d UID;", i)))
		if err != nil {
			fmt.Printf("write err %v\n", err)
			continue
		}
		buf := make([]byte, 128)
		N, err := readPort(s, buf)
		if err != nil {
			fmt.Printf("read err %v\n", err)
			continue
		}
		fmt.Printf("answer rs %d, n = %d, %s\n", i, N, string(buf))
		if N < 2 {
			continue
		}
		ans := string(buf[0:N])
		foundStrings := h.uidAnswer.FindStringSubmatch(ans)
		if len(foundStrings) < 2 {
			continue
		}

		fmt.Printf("uid is [%s]\n", foundStrings[1])
		board := NewRev2BoardRS(key, s, h.hwMetrics)
		port := NewPorts(key, s)
		h.addRev2BoardRS(board)
		h.addPort(key, port)
		return board.Run()
	}
	s.Close()
	return app.ErrNotFoundBoard
}

func (h *HardwareAccessLayer) addRev2BoardRS(board *Rev2BoardRS) {
	h.portsMu.Lock()
	h.portRev2BoardRS = append(h.portRev2BoardRS, board)
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) runOnRev2BoardRS(cfg app.RelayConfig) error {
	h.portsMu.Lock()
	defer h.portsMu.Unlock()
	for i := range h.portRev2BoardRS {
		_, ok := h.portRev2BoardRS[i].stationNumber[int(cfg.StationID)]
		if ok {
			h.portRev2BoardRS[i].RunConfig(cfg)
			return nil
		}

	}
	return app.ErrNotFoundBoard
}
