package app

import (
	"errors"
)

const MAX_ALLOWED_DEVICES = 6

var (
	ErrNotFound            = errors.New("not found")
	ErrNotFoundDispenser   = errors.New("not found Dispenser")
	ErrNotFoundBoard       = errors.New("not found Board")
	ErrNonFreezing         = errors.New("the non-freezing is over")
	ErrInCommandFok        = errors.New("error in command FOK")
	ErrReadAnswerDispenser = errors.New("error in read answer from Dispenser")
	ErrDispenserNotRespond = errors.New("dispenser is not responding")
	ErrModelDoesNotExist   = errors.New("model does not exist")
	ErrWrongIndex          = errors.New("wrong index")
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
	DeviceInfo() string
}

// ControlBoard represents one board (even virtual) to control relays
type ControlBoard interface {
	StopAll() error
	Port() string
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

type HardwareMetrics struct {
	MotorDetected   GaugeMetric
	MotorSerialPort GaugeMetric
	BoardDetected   GaugeMetric

	// MotorSpeedGauge Motor actual speed by id
	MotorSpeedGauge GaugeMetric

	// MotorDesiredSpeedGauge Motor desired speed by id
	MotorDesiredSpeedGauge GaugeMetric

	// MotorRunning indicates 1 for running motor and zero for stopped. By id.
	MotorRunning GaugeMetric

	// RS485MotorRequestCounter, Total number of requests to a motor by id
	RS485MotorRequestCounter     CounterMetric
	RS485MotorRequestFailCounter CounterMetric
}

type GaugeMetric interface {
	SetGaugeByID(id string, newValue float64)
}

type CounterMetric interface {
	Inc(id string)
}
