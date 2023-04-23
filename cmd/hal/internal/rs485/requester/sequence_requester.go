package requester

import (
	"context"
	"errors"
	"fmt"
	"sync"
)

const MAX_ALLOWED_DEVICES = 5

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

type MotorError struct {
	Comment string
	Code    int
	Count   int
}

type RequestType uint8

var (
	ErrPoolExhausted      = errors.New("pool exhausted")
	ErrUnknownRequestType = errors.New("unknown request type")
)

const (
	RequestTypeGetMaxSpeed RequestType = iota
	RequestTypeSetSpeedPercent
	RequestTypeGetSpeedPercent
	RequestTypeStartMotor
	RequestTypeStopMotor
	RequestTypeTemperature
)

const (
	commandsInBuffer       = 100
	maxSpeedCode     uint8 = 0x04
)

// RequestWrapper or now its extremly simple just to be working
type RequestWrapper struct {
	answerChan      chan Answer
	answerChanMutex sync.RWMutex
	kernel          Request
}

type Request struct {
	T        RequestType
	DeviceID uint8
	Val      uint16
}

func (r *RequestWrapper) SetAnswerChannel(answerChan chan Answer) {
	r.answerChanMutex.Lock()
	r.answerChan = answerChan
	r.answerChanMutex.Unlock()
}

func (r *RequestWrapper) AnswerChannel(answerChan chan Answer) (chan Answer, error) {
	r.answerChanMutex.RLock()
	defer r.answerChanMutex.RUnlock()
	return r.answerChan, nil
}

// Answer for now its an extremely simple just to show its working
type Answer struct {
	R   Request
	Err error
	Val float64
}

func NewRequestWrapper(request Request) *RequestWrapper {
	return &RequestWrapper{
		kernel: request,
	}
}

func NewRequest(t RequestType, deviceID uint8, val uint16) Request {
	return Request{
		T:        t,
		DeviceID: deviceID,
		Val:      val,
	}
}

type SequenceRequester struct {
	ctx      context.Context
	waitChan chan struct{}

	highPriorityQueue chan *RequestWrapper
	lowPriorityQueue  chan *RequestWrapper

	AnswerChannels      []chan Answer
	AnswerChannelsMutex sync.RWMutex

	devs      [MAX_ALLOWED_DEVICES + 1]bool
	devsMutex sync.RWMutex

	driver MotorDriver
}

func (s *SequenceRequester) Port() string {
	return s.driver.Port()
}

func (s *SequenceRequester) Destroy() {
	s.driver.Destroy()
	close(s.waitChan)
}

func (s *SequenceRequester) HasDevice(deviceID uint8) bool {
	if deviceID > MAX_ALLOWED_DEVICES {
		return false
	}
	s.devsMutex.RLock()
	res := s.devs[deviceID]
	s.devsMutex.RUnlock()
	return res
}

// loop must be a private function. It must check
func (s *SequenceRequester) loop() int {
	select {
	case request := <-s.highPriorityQueue:
		s.processRequest(request)
		return 1
	case <-s.waitChan:
		return -1
	default:
		select {
		case request := <-s.highPriorityQueue:
			s.processRequest(request)
			return 1
		case request := <-s.lowPriorityQueue:
			s.processRequest(request)
			return 0
		case <-s.waitChan:
			return -1
		}
	}
}

func (s *SequenceRequester) processRequest(requestWrapper *RequestWrapper) {
	switch requestWrapper.kernel.T {
	case RequestTypeGetMaxSpeed:
		val, err := s.driver.MaxSpeed(requestWrapper.kernel.DeviceID)
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: float64(val),
		}
		requestWrapper.answerChan <- res
		return
	case RequestTypeStartMotor:
		err := s.driver.StartMotor(requestWrapper.kernel.DeviceID)
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: 0x0,
		}
		requestWrapper.answerChan <- res
		return
	case RequestTypeStopMotor:
		err := s.driver.StopMotor(requestWrapper.kernel.DeviceID)
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: 0x0,
		}
		requestWrapper.answerChan <- res
		return
	case RequestTypeSetSpeedPercent:
		err := s.driver.SetSpeedPercent(requestWrapper.kernel.DeviceID, int16(requestWrapper.kernel.Val))
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: 0x0,
		}
		requestWrapper.answerChan <- res
		return
	case RequestTypeGetSpeedPercent:
		val, err := s.driver.GetSpeedPercent(requestWrapper.kernel.DeviceID)
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: float64(val),
		}
		requestWrapper.answerChan <- res
		return
	case RequestTypeTemperature:
		val, err := s.driver.Temperature(requestWrapper.kernel.DeviceID)
		res := Answer{
			R:   requestWrapper.kernel,
			Err: err,
			Val: float64(val),
		}
		requestWrapper.answerChan <- res
		return
	default:
		res := Answer{
			R:   requestWrapper.kernel,
			Err: ErrUnknownRequestType,
			Val: 0x00,
		}
		requestWrapper.answerChan <- res
	}
}

func NewSequenceRequester(ctx context.Context, motorDriver MotorDriver) *SequenceRequester {
	res := &SequenceRequester{
		ctx:               ctx,
		highPriorityQueue: make(chan *RequestWrapper, commandsInBuffer),
		lowPriorityQueue:  make(chan *RequestWrapper, commandsInBuffer),
		AnswerChannels:    make([]chan Answer, 0, 2*commandsInBuffer),
		driver:            motorDriver,
		waitChan:          make(chan struct{}),
	}
	N := cap(res.AnswerChannels)
	for i := 0; i < N; i++ {
		res.AnswerChannels = append(res.AnswerChannels, make(chan Answer, 1))
	}
	return res
}

func (s *SequenceRequester) Run() {
	go s.workingLoop()
}

func (s *SequenceRequester) workingLoop() {
	for {
		select {
		case <-s.waitChan:
			return
		default:
			s.loop()
		}
	}
}
func (s *SequenceRequester) PullAnswerChannel() (chan Answer, error) {
	s.AnswerChannelsMutex.Lock()
	defer s.AnswerChannelsMutex.Unlock()
	lastIndex := len(s.AnswerChannels) - 1
	if lastIndex < 0 {
		return nil, ErrPoolExhausted
	}
	res := s.AnswerChannels[lastIndex]
	s.AnswerChannels = s.AnswerChannels[:lastIndex]
	return res, nil
}

func (s *SequenceRequester) ScanDevices(attempts int) int {
	res := 0
	for i := uint8(1); i <= MAX_ALLOWED_DEVICES; i++ {
		fmt.Printf("trying device #%d\n", i)
		deviceAnswered := s.SpecificDeviceReplies(i, attempts)
		fmt.Printf("trying device #%d, answered=%+v\n", i, deviceAnswered)
		if deviceAnswered {
			res++
		}
		s.setDeviceAvailability(i, deviceAnswered)
	}
	return res
}

func (s *SequenceRequester) setDeviceAvailability(deviceID uint8, availability bool) {
	if deviceID > MAX_ALLOWED_DEVICES {
		return
	}
	s.devsMutex.Lock()
	s.devs[deviceID] = availability
	s.devsMutex.Unlock()
}

func (s *SequenceRequester) SpecificDeviceReplies(deviceID uint8, attempts int) bool {
	for i := 0; i < attempts; i++ {
		fmt.Printf("dev #%d, attempt #%d low get max speed\n", deviceID, i)
		a := s.LowPriorityGetMaxSpeed(deviceID)
		if a.Err == nil {
			return true
		}
	}
	return false
}

func (s *SequenceRequester) PushAnswerChannel(answerChan chan Answer) {
	s.AnswerChannelsMutex.Lock()
	defer s.AnswerChannelsMutex.Unlock()
	s.AnswerChannels = append(s.AnswerChannels, answerChan)
}

func (s *SequenceRequester) LowPriorityGetMaxSpeed(deviceID uint8) Answer {
	request := NewRequest(RequestTypeGetMaxSpeed, deviceID, 0x00)

	return s.runRequest(s.lowPriorityQueue, request)
}

func (s *SequenceRequester) HighPrioritySetSpeedPercent(deviceID uint8, percent int) Answer {
	request := NewRequest(RequestTypeSetSpeedPercent, deviceID, uint16(percent))
	return s.runRequest(s.highPriorityQueue, request)
}

func (s *SequenceRequester) HighPriorityGetSpeedPercent(deviceID uint8) Answer {
	request := NewRequest(RequestTypeGetSpeedPercent, deviceID, 0x00)
	return s.runRequest(s.highPriorityQueue, request)
}
func (s *SequenceRequester) LowPriorityGetSpeedPercent(deviceID uint8) Answer {
	request := NewRequest(RequestTypeGetSpeedPercent, deviceID, 0x00)
	return s.runRequest(s.lowPriorityQueue, request)
}

func (s *SequenceRequester) HighPriorityStartMotor(deviceID uint8) Answer {
	request := NewRequest(RequestTypeStartMotor, deviceID, 0x00)
	return s.runRequest(s.highPriorityQueue, request)
}

func (s *SequenceRequester) HighPriorityStopMotor(deviceID uint8) Answer {
	request := NewRequest(RequestTypeStopMotor, deviceID, 0x00)
	return s.runRequest(s.highPriorityQueue, request)
}

func (s *SequenceRequester) runRequest(currentQueue chan *RequestWrapper, request Request) Answer {
	curChan, err := s.PullAnswerChannel()
	if err != nil {
		return Answer{
			R:   request,
			Err: err,
		}
	}
	requestWrapper := NewRequestWrapper(request)
	requestWrapper.SetAnswerChannel(curChan)
	currentQueue <- requestWrapper
	answer := <-curChan
	requestWrapper.SetAnswerChannel(nil)
	s.PushAnswerChannel(curChan)
	return answer
}
