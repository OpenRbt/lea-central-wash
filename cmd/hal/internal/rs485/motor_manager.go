package rs485

import (
	"context"
	"fmt"
	"strconv"
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/requester"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/rsutil"
)

const (
	attemptsToPingDevice     = 20
	requestersToDeleteMaxCnt = 100
)

// MotorManager manages motors
type MotorManager struct {
	ctx          context.Context
	refreshDelay time.Duration

	running      bool
	runningMutex sync.RWMutex

	sequenceRequesters     []*requester.SequenceRequester
	sequenceRequesterMutex sync.RWMutex

	portReporter requester.PortReporter

	requesterToDelete   chan *requester.SequenceRequester
	hwMetrics           app.HardwareMetrics
	devicesModel        FreqGenModel
	lastReportedDevices app.DevicesList
}

func NewMotorManager(ctx context.Context, hwMetrics app.HardwareMetrics,
	portReporter requester.PortReporter, refreshDelay time.Duration, newModel FreqGenModel) *MotorManager {
	return &MotorManager{
		ctx:               ctx,
		refreshDelay:      refreshDelay,
		portReporter:      portReporter,
		hwMetrics:         hwMetrics,
		devicesModel:      newModel,
		requesterToDelete: make(chan *requester.SequenceRequester, requestersToDeleteMaxCnt),
	}
}

func (m *MotorManager) TryAddDevice(devName string) error {
	requester, err := m.CheckAndGetSequenceRequencerPort(devName)
	if err != nil {
		return err
	}
	requester.Run()
	fmt.Printf("adding requester on %s \n", devName)
	m.AddSequenceRequester(requester)
	return nil
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

func (m *MotorManager) StopMotorAttempts(requester *requester.SequenceRequester, deviceID uint8, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		m.hwMetrics.RS485MotorRequestCounter.Inc(strconv.Itoa(int(deviceID)))
		a := requester.HighPriorityStopMotor(deviceID)
		if a.Err == nil {
			return nil
		} else {
			m.hwMetrics.RS485MotorRequestFailCounter.Inc(strconv.Itoa(int(deviceID)))
			lastError = a.Err
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
		// fmt.Printf("RS dev NOT found #%d\n", deviceID)
		return ErrDeviceNotFound
	} else {
		fmt.Printf("RS dev found #%d\n", deviceID)
	}
	return nil
}

func (m *MotorManager) StartMotorAttempts(requester *requester.SequenceRequester, deviceID uint8, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		m.hwMetrics.RS485MotorRequestCounter.Inc(strconv.Itoa(int(deviceID)))
		a := requester.HighPriorityStartMotor(deviceID)
		if a.Err == nil {
			return nil
		} else {
			m.hwMetrics.RS485MotorRequestFailCounter.Inc(strconv.Itoa(int(deviceID)))
			lastError = a.Err
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

func (m *MotorManager) SetSpeedPercentAttempts(requester *requester.SequenceRequester, deviceID uint8, percent int16, attempts int) error {
	var lastError error
	for attempt := 0; attempt < attemptsToPingDevice; attempt++ {
		m.hwMetrics.RS485MotorRequestCounter.Inc(strconv.Itoa(int(deviceID)))
		a := requester.HighPrioritySetSpeedPercent(deviceID, int(percent))
		if a.Err == nil {
			return nil
		} else {
			lastError = a.Err
			m.hwMetrics.RS485MotorRequestFailCounter.Inc(strconv.Itoa(int(deviceID)))
		}
	}
	return lastError
}

func (m *MotorManager) SequenceRequester(i int) *requester.SequenceRequester {
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

func (m *MotorManager) AddSequenceRequester(r *requester.SequenceRequester) {
	m.sequenceRequesterMutex.Lock()
	defer m.sequenceRequesterMutex.Unlock()
	m.sequenceRequesters = append(m.sequenceRequesters, r)
}

func (m *MotorManager) RemoveSequenceRequester(k *requester.SequenceRequester) {
	// Let's delete the requester
	fmt.Println("mm requester to delete")
	m.sequenceRequesterMutex.Lock()
	for i := range m.sequenceRequesters {
		if m.sequenceRequesters[i] == k {
			lastElementIndex := len(m.sequenceRequesters) - 1
			m.sequenceRequesters[i] = m.sequenceRequesters[lastElementIndex]
			m.sequenceRequesters = m.sequenceRequesters[:lastElementIndex]
		}
		k.Destroy()
	}
	if m.portReporter != nil {
		m.portReporter.FreePort(k.Port())
	}

	m.sequenceRequesterMutex.Unlock()
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
			fmt.Println("mm ctx done")
			m.Destroy()
			return
		case <-timer.C:
			fmt.Println("mm time manager")
			timer.Stop()
			m.collectInfoIteration()
			timer.Reset(m.refreshDelay)
		case k := <-m.requesterToDelete:
			m.RemoveSequenceRequester(k)
		}
	}
}

func (m *MotorManager) collectInfoIteration() int8 {
	fmt.Print("collect info iteration\n")
	m.sequenceRequesterMutex.RLock()
	N := len(m.sequenceRequesters)
	m.sequenceRequesterMutex.RUnlock()

	fmt.Printf("sequence requesters count:%d \n", N)
	finalDevicesList := app.NewDeviceList()

	for i := 0; i <= N; i++ {
		var curRequester *requester.SequenceRequester
		curRequester = nil
		m.sequenceRequesterMutex.RLock()
		if i < len(m.sequenceRequesters) {
			curRequester = m.sequenceRequesters[i]
		}
		m.sequenceRequesterMutex.RUnlock()
		if curRequester == nil {
			break // means we reached end of the cycle
		}
		fmt.Printf("scan devices engaged\n")
		devicesFound := curRequester.ScanDevices(3, finalDevicesList)
		finalDevicesList.AddRange(devicesFound)
		if devicesFound.Count() == 0 {
			fmt.Printf("no devices found")
			m.MarkSequenceRequesterToDelete(curRequester)
		}
	}

	m.lastReportedDevices.Clean()
	m.lastReportedDevices.AddRange(finalDevicesList)

	for i := int8(1); i <= app.MAX_ALLOWED_DEVICES; i++ {
		devFound := 0
		if finalDevicesList.Contains(i) {
			devFound = 1
		}
		m.hwMetrics.MotorDetected.SetGaugeByID(strconv.Itoa(int(i)), float64(devFound))
	}

	fmt.Printf("found %d devices, from %d requests\n", finalDevicesList.Count(), N)
	return finalDevicesList.Count()
}

func (m *MotorManager) MarkSequenceRequesterToDelete(r *requester.SequenceRequester) {
	fmt.Printf("Marked to delete\n")
	m.requesterToDelete <- r
}

func (m *MotorManager) Destroy() {
	m.sequenceRequesterMutex.Lock()
	defer m.sequenceRequesterMutex.Unlock()
	for i := range m.sequenceRequesters {
		m.sequenceRequesters[i].Destroy()
		m.portReporter.FreePort(m.sequenceRequesters[i].Port())
	}
}

func (m *MotorManager) CheckAndGetSequenceRequencerPort(port string) (*requester.SequenceRequester, error) {
	// Let's create a device

	cfg := rsutil.NewRS485Config(port, 19200, 10000)
	mDriver, err := CreateFrequencyGenerator(m.devicesModel, cfg) // 10000 means 100.00 % for our driver
	if err != nil {
		return nil, fmt.Errorf("can't initialize newfrequencygenerator %+w", err)
	}

	deviceFound := false
	for i := uint8(1); i < app.MAX_ALLOWED_DEVICES; i++ {
		maxSpeed, err := mDriver.MaxSpeed(i)
		if err != nil { //Let's just do 2 attempts
			maxSpeed, err = mDriver.MaxSpeed(i)
		}
		if err != nil {
			continue
		}
		if maxSpeed > 0 {
			deviceFound = true
			break
		}
	}
	if !deviceFound {
		err := mDriver.Destroy()
		if err != nil {
			fmt.Printf("cant properly destroy device on %s\n", port)
		}
		return nil, ErrDeviceNotFound
	}

	sRequester := requester.NewSequenceRequester(m.ctx, mDriver)
	return sRequester, nil
}
