package seqrequests

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/modbusae200h"
)

const (
	attemptsToPingDevice     = 3
	requestersToDeleteMaxCnt = 100
)

// MotorManager manages motors
type MotorManager struct {
	ctx          context.Context
	refreshDelay time.Duration

	running      bool
	runningMutex sync.RWMutex

	sequenceRequesters     []*SequenceRequester
	sequenceRequesterMutex sync.RWMutex

	portReporter PortReporter

	requesterToDelete chan *SequenceRequester
}

func NewMotorManager(ctx context.Context, portReporter PortReporter, refreshDelay time.Duration) *MotorManager {
	return &MotorManager{
		ctx:               ctx,
		refreshDelay:      refreshDelay,
		portReporter:      portReporter,
		requesterToDelete: make(chan *SequenceRequester, requestersToDeleteMaxCnt),
	}
}

func (m *MotorManager) StopMotor(deviceID uint8) error {
	N := m.SequenceRequesterCount()
	var lastError error
	deviceFound := false
	for i := 0; i < N; i++ {
		requester := m.SequenceRequester(i)
		if requester != nil && requester.HasDevice(deviceID) {
			lastError = m.StopMotorAttempts(requester, deviceID, attemptsToPingDevice)
			if lastError != nil {
				fmt.Printf("can't stop motor, all attempts exhausted %+v\n", lastError)
				continue
			}
			deviceFound = true
		}
	}
	if !deviceFound {
		return ErrDeviceNotFound
	}
	return nil
}

func (m *MotorManager) StopMotorAttempts(requester *SequenceRequester, deviceID uint8, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		a := requester.HighPriorityStopMotor(deviceID)
		if a.err == nil {
			return nil
		} else {
			lastError = a.err
		}
	}
	return lastError
}

func (m *MotorManager) StartMotor(deviceID uint8) error {
	N := m.SequenceRequesterCount()
	deviceFound := false
	var lastError error
	for i := 0; i < N; i++ {
		requester := m.SequenceRequester(i)
		if requester != nil && requester.HasDevice(deviceID) {
			lastError = m.StartMotorAttempts(requester, deviceID, attemptsToPingDevice)
			if lastError != nil {
				fmt.Printf("can't start motor, all attempts exhausted %+v\n", lastError)
				continue
			}
			deviceFound = true
		}
	}
	if !deviceFound {
		return ErrDeviceNotFound
	}
	return nil
}

func (m *MotorManager) StartMotorAttempts(requester *SequenceRequester, deviceID uint8, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		a := requester.HighPriorityStartMotor(deviceID)
		if a.err == nil {
			return nil
		} else {
			lastError = a.err
		}
	}
	return lastError
}

func (m *MotorManager) SetSpeedPercent(deviceID uint8, percent int16) error {
	N := m.SequenceRequesterCount()
	var lastError error
	deviceFound := false
	for i := 0; i < N; i++ {
		requester := m.SequenceRequester(i)
		if requester != nil && requester.HasDevice(deviceID) {
			lastError = m.SetSpeedPercentAttempts(requester, deviceID, percent, attemptsToPingDevice)
			if lastError != nil {
				fmt.Printf("can't start motor, all attempts exhausted %+v\n", lastError)
				continue
			}
			deviceFound = true
		}
	}
	if !deviceFound {
		return ErrDeviceNotFound
	}
	return nil
}

func (m *MotorManager) SetSpeedPercentAttempts(requester *SequenceRequester, deviceID uint8, percent int16, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		a := requester.HighPrioritySetSpeedPercent(deviceID, int(percent))
		if a.err == nil {
			return nil
		} else {
			lastError = a.err
		}
	}
	return lastError
}

func (m *MotorManager) SequenceRequester(i int) *SequenceRequester {
	m.sequenceRequesterMutex.RLock()
	defer m.sequenceRequesterMutex.RUnlock()
	if i < len(m.sequenceRequesters) {
		return m.sequenceRequesters[i]
	}
	return nil
}

func (m *MotorManager) SequenceRequesterCount() int {
	m.sequenceRequesterMutex.RLock()
	res := len(m.sequenceRequesters)
	m.sequenceRequesterMutex.RUnlock()
	return res
}

func (m *MotorManager) AddSequenceRequester(r *SequenceRequester) {
	m.sequenceRequesterMutex.Lock()
	defer m.sequenceRequesterMutex.Unlock()
	m.sequenceRequesters = append(m.sequenceRequesters, r)
	r.Run()
}

func (m *MotorManager) Run() error {
	m.runningMutex.Lock()
	defer m.runningMutex.Unlock()
	if m.running {
		return ErrAlreadyRunning
	}

	go m.workingLoop()
	return nil
}

func (m *MotorManager) workingLoop() {
	// first of all we must scan everything to start
	m.collectInfoIteration()

	timer := time.NewTimer(m.refreshDelay)
	for {
		select {
		case <-m.ctx.Done():
			m.Destroy()
			return
		case <-timer.C:
			timer.Stop()
			m.collectInfoIteration()
			timer.Reset(m.refreshDelay)
		case k := <-m.requesterToDelete:
			// Let's delete the requester
			m.sequenceRequesterMutex.Lock()
			for i := range m.sequenceRequesters {
				if m.sequenceRequesters[i] == k {
					lastElementIndex := len(m.sequenceRequesters) - 1
					m.sequenceRequesters[i] = m.sequenceRequesters[lastElementIndex]
					m.sequenceRequesters = m.sequenceRequesters[:lastElementIndex]
				}
				k.Destroy()
			}
			m.portReporter.FreePort(k.Port())
			m.sequenceRequesterMutex.Unlock()
		}
	}
}

func (m *MotorManager) collectInfoIteration() int {
	fmt.Print("collect info iteration\n")
	m.sequenceRequesterMutex.RLock()
	N := len(m.sequenceRequesters)
	m.sequenceRequesterMutex.RUnlock()

	totalDevicesFound := 0
	for i := 0; i <= N; i++ {
		var curRequester *SequenceRequester
		curRequester = nil
		m.sequenceRequesterMutex.RLock()
		if i < len(m.sequenceRequesters) {
			curRequester = m.sequenceRequesters[i]
		}
		m.sequenceRequesterMutex.RUnlock()
		if curRequester == nil {
			break // means we reached end of the cycle
		}
		devicesFound := curRequester.ScanDevices(attemptsToPingDevice)
		if devicesFound == 0 {
			m.MarkSequenceRequesterToDelete(curRequester)
		}
		totalDevicesFound += devicesFound
	}
	fmt.Printf("found %d devices, from %d requestes\n", totalDevicesFound, N)
	return totalDevicesFound
}

func (m *MotorManager) MarkSequenceRequesterToDelete(r *SequenceRequester) {
	fmt.Printf("Marked to delete\n")
	m.requesterToDelete <- r
}

func (m *MotorManager) Destroy() {
	for i := range m.sequenceRequesters {
		m.sequenceRequesters[i].Destroy()
		m.portReporter.FreePort(m.sequenceRequesters[i].Port())
	}
}

func (m *MotorManager) CheckAndGetSequenceRequencerPort(port string) (*SequenceRequester, error) {
	// Let's create a device
	mDriver, err := modbusae200h.NewFrequencyGenerator(port, 9600, 5000)
	if err != nil {
		return nil, fmt.Errorf("can't initialize newfrequencygenerator %+w", err)
	}

	deviceFound := false
	for i := uint8(1); i < 20; i++ {
		maxSpeed, err := mDriver.MaxSpeed(i)
		if err != nil { //Let's just do 2 attempts
			maxSpeed, err = mDriver.MaxSpeed(i)
		}
		if err != nil {
			continue
		}
		if maxSpeed > 0 {
			break
		}
	}
	if !deviceFound {
		err := mDriver.Destroy()
		if err != nil {
			fmt.Printf("cant properly destroy device on %s\n", port)
		}
	}

	sRequester := NewSequenceRequester(m.ctx, mDriver)
	return sRequester, nil
}
