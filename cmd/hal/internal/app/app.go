package app

import (
	"errors"
)

var (
	ErrNotFound = errors.New("not found")
)

// HardwareAccessLayer describes an interface to access hardware control modules
type HardwareAccessLayer interface {
	Start()
	ControlBoard(key int32) (ControlBoard, error)
	RunProgram(id int32, cfg RelayConfig) (err error)
	MeasureVolumeMilliliters(cmd int) error
	Volume() DispenserStatus
	GetLevel() int
	ProgramStop() error
}

// ControlBoard represents one board (even virtual) to control relays
type ControlBoard interface {
	StopAll() error
	MyPosition() (int, error)
	RunConfig(config RelayConfig)
}

// RelayConfig represents a relay config for something
type RelayConfig struct {
	// MotorSpeedPercent  will be passed to ESQ500/600 or another frequency controller to change the motor speed
	// NOT MORE THAN 150 PERCENT!!!
	MotorSpeedPercent int32
	// If anything happens with the whole system, the control board will stop all after this time
	// NOT MORE THAN 3600 SECONDS!!!
	TimeoutSec int32
	// Timings are settings for actual relays
	Timings []Relay
}

// Relay is a config for a relay
type Relay struct {
	ID      int
	TimeOn  int
	TimeOff int
}

type DispenserStatus struct {
	Milliliters           int64
	ErrorCommandDispenser error
}
