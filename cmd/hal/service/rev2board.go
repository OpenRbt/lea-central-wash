package service

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"

	"github.com/tarm/serial"
)

// Rev2Board describes Revision 2 openrbt.com board
type Rev2Board struct {
	osPath        string
	openPort      *serial.Port
	toRemove      bool
	stationNumber int
	errorCount    int
	Commands      chan app.RelayConfig
	hwMetrics     app.HardwareMetrics
}

// NewRev2Board is a constructor, using USB to communicate
func NewRev2Board(osPath string, openPort *serial.Port, hwMetrics app.HardwareMetrics) *Rev2Board {
	return &Rev2Board{
		osPath:        osPath,
		openPort:      openPort,
		stationNumber: -1,
		toRemove:      false,
		Commands:      make(chan app.RelayConfig),
		hwMetrics:     hwMetrics,
	}
}

// Run just runs a goroutine which controls a device
func (r *Rev2Board) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev2Board) workingLoop() {
	duration := 800 * time.Millisecond
	timer := time.NewTimer(duration)
	var cmd app.RelayConfig
	for {
		select {
		case <-timer.C:
			timer.Stop()
			stationNumber, err := r.SendPing()
			timer.Reset(duration)
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
				if r.errorCount > 1 {
					r.reconnect()
				}
				err = r.runCommand(cmd)
			}
		}
	}
}

func (r *Rev2Board) reconnect() error {
	r.hwMetrics.Rev2BoardReconnectCounter.Inc(strconv.Itoa(r.stationNumber))
	r.openPort.Close()
	time.Sleep(20 * time.Millisecond)
	c := &serial.Config{Name: "/dev/" + r.osPath, Baud: 38400, ReadTimeout: time.Millisecond * 100}
	s, err := serial.OpenPort(c)
	if err == nil {
		r.openPort = s
		fmt.Printf("reconnect OK\n")
	} else {
		fmt.Printf("reconnect err: %s\n", err)
	}
	return err
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
		fmt.Println(err)
		return err
	}

	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
		fmt.Println(err)
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

// RunConfig just runs a config
func (r *Rev2Board) RunConfig(config app.RelayConfig) {
	r.Commands <- config
}

func (r *Rev2Board) Port() string {
	return r.osPath
}
