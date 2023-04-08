package seqrequests

import (
	"errors"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type DummyPortReporter struct {
}

func (d *DummyPortReporter) FreePort(port string) {
	fmt.Printf("Port %s destroyed\n", port)
}

type DummyMotor struct {
	currentSpeed int
	maxSpeed     int
	nominalSpeed int
	errorCycle   int
	stopped      bool
	errors       map[uint8][]MotorError

	killed      bool
	killedMutex sync.RWMutex
}

var (
	ErrRandomHappened     = errors.New("predefined random test error happened")
	ErrDeliberateHappened = errors.New("predefined deliberate error happened")
	ErrNominalSpeedZero   = errors.New("nominal speed must not be zero")
	ErrAlreadyRunning     = errors.New("service already running")
	ErrDeviceNotFound     = errors.New("device is not found")
)

func NewDummyMotor(nominalSpeed, maxSpeed int) *DummyMotor {
	return &DummyMotor{
		currentSpeed: 0,
		maxSpeed:     maxSpeed,
		nominalSpeed: nominalSpeed,
	}
}

func (m *DummyMotor) Kill() {

	m.killedMutex.Lock()
	m.killed = true
	fmt.Print("motor killed\n")
	m.killedMutex.Unlock()
}

func (m *DummyMotor) NominalSpeed() uint16 {
	return uint16(m.nominalSpeed)
}
func (m *DummyMotor) PushError(deviceID uint8, err error) {
	m.errors[deviceID] = append(m.errors[deviceID], MotorError{
		Comment: err.Error(),
		Code:    0x01,
	})
}

func (m *DummyMotor) RandError(deviceID uint8) error {
	mode := m.errorCycle % 10
	m.errorCycle = m.errorCycle + 1
	if mode == 0 {
		randNum := rand.Intn(1024) % 2
		if randNum > 0 {
			return nil
		}
		return ErrRandomHappened
	} else if mode == 1 {
		return ErrDeliberateHappened
	} else {
		return nil
	}
}
func (m *DummyMotor) StopMotor(device uint8) error {
	time.Sleep(time.Millisecond * 10)
	err := m.RandError(device)
	if err != nil {
		return err
	}
	m.stopped = true
	return nil
}

func (m *DummyMotor) StartMotor(device uint8) error {
	time.Sleep(time.Millisecond * 10)
	err := m.RandError(device)
	if err != nil {
		return err
	}
	m.stopped = false
	return nil
}

func (m *DummyMotor) SetSpeedPercent(device uint8, percent int16) error {
	time.Sleep(time.Millisecond * 12)
	err := m.RandError(device)
	if err != nil {
		return err
	}
	if percent > 100 {
		percent = 100
	}
	if percent < 0 {
		percent = 0
	}
	var hz float32 = float32(m.nominalSpeed)
	hz = hz * float32(percent)
	hz = hz / 100
	m.currentSpeed = int(hz)
	fmt.Printf("dummy motor #%d speed set %d\n", device, m.currentSpeed)
	return nil
}

func (m *DummyMotor) GetSpeedPercent(device uint8) (int16, error) {
	time.Sleep(time.Millisecond * 10)
	var cur float32 = float32(m.currentSpeed)
	var nominal float32 = float32(m.nominalSpeed)
	if m.nominalSpeed != 0 {
		percent := 100.0 * cur / nominal
		fmt.Printf("dummy motor #%d speed get %d\n", device, int(percent))
		return int16(percent), nil
	}
	return 0, ErrDeliberateHappened
}
func (m *DummyMotor) MaxSpeed(device uint8) (uint16, error) {
	m.killedMutex.RLock()
	isKilled := m.killed
	m.killedMutex.RUnlock()
	if isKilled {
		return 0, ErrDeliberateHappened
	}
	if device > 6 {
		return 0, ErrDeliberateHappened
	}
	time.Sleep(time.Millisecond * 10)
	err := m.RandError(device)
	if err != nil {
		return 0, err
	}
	fmt.Printf("dummy motor #%d max speed get %d\n", device, int(m.maxSpeed))
	return uint16(m.maxSpeed), nil
}

func (m *DummyMotor) Port() string {
	return "ttyUSB"
}

func (m *DummyMotor) Destroy() error {
	fmt.Println("dummy motor destroyed")
	return nil
}

func (m *DummyMotor) Temperature(device uint8) (float32, error) {
	time.Sleep(time.Millisecond * 10)
	err := m.RandError(device)
	if err != nil {
		return 0, err
	}
	return 55.0, nil
}
