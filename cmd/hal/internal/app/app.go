package app

import (
	"errors"
)

var (
	ErrNotFound            = errors.New("not found")
	ErrNotFoundDispenser   = errors.New("not found Dispenser")
	ErrNotFoundBoard       = errors.New("not found Board")
	ErrNonFreezing         = errors.New("the non-freezing is over")
	ErrInCommandFok        = errors.New("error in command FOK")
	ErrReadAnswerDispenser = errors.New("error in read answer from Dispenser")
	ErrDispenserNotRespond = errors.New("dispenser is not responding")
)

type MotorDriver interface {
	StopMotor(device uint8) error
	StartMotor(device uint8) error
	SetSpeedPercent(device uint8, percent int16) error
	GetSpeedPercent(device uint8) (int16, error)
	MaxSpeed(device uint8) (uint16, error)
	Temperature(device uint8) (float32, error)
	Destroy() error
	Port() string
}

// HardwareAccessLayer describes an interface to access hardware control modules
type HardwareAccessLayer interface {
	Start()
	ControlBoard(key int32) (ControlBoard, error)
	RunProgram(id int32, cfg RelayConfig) (err error)
	MeasureVolumeMilliliters(measureVolume int, stationID int32, startCfg RelayConfig, stopCfg RelayConfig) error
	Volume() DispenserStatus
	GetLevel() int
	DispenserStop(cfg RelayConfig) error
	FreePort(portName string)
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
