package service

import (
	"errors"
	"log"
	"sync"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"
)

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

// StopAll just stops all relays
func (r *Rev2DebugBoard) StopAll() error {
	// Please add here RunCommand with all zero relays
	return errors.New("not implemented")
}
