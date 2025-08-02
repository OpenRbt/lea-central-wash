package service

import (
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"

	"github.com/tarm/serial"
)

const readTimeout = 100 * time.Millisecond

// Rev2BoardRS describes Revision 2 openrbt.com board
type Rev2BoardRS struct {
	osPath        string
	openPort      *serial.Port
	toRemove      bool
	stationNumber map[int]bool
	errorCount    int
	Commands      chan app.RelayConfig
	hwMetrics     app.HardwareMetrics
}

// NewRev2BoardRS is a constructor, using USB to communicate
func NewRev2BoardRS(osPath string, openPort *serial.Port, hwMetrics app.HardwareMetrics) *Rev2BoardRS {
	return &Rev2BoardRS{
		osPath:    osPath,
		openPort:  openPort,
		toRemove:  false,
		Commands:  make(chan app.RelayConfig),
		hwMetrics: hwMetrics,
	}
}

// Run just runs a goroutine which controls a device
func (r *Rev2BoardRS) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev2BoardRS) workingLoop() {
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

func (r *Rev2BoardRS) reconnect() error {
	r.hwMetrics.Rev2BoardRSReconnectCounter.Inc(r.osPath)
	r.openPort.Close()
	time.Sleep(20 * time.Millisecond)
	c := &serial.Config{Name: "/dev/" + r.osPath, Baud: 9600, ReadTimeout: time.Millisecond * 50}
	s, err := serial.OpenPort(c)
	if err == nil {
		r.openPort = s
		fmt.Printf("reconnect OK\n")
	} else {
		fmt.Printf("reconnect err: %s\n", err)
	}
	return err
}

func (r *Rev2BoardRS) runCommand(cmd app.RelayConfig) error {
	var cmdBuf strings.Builder
	cmdBuf.Grow(192)
	finalRelays := make(map[int]app.Relay, 12)
	for _, relayItem := range cmd.Timings {
		finalRelays[relayItem.ID] = relayItem
	}
	cmdBuf.WriteString(fmt.Sprintf("S%d RUN A-|", cmd.StationID))
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
	N, err := readPort(r.openPort, buf)
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
//func (r *Rev2BoardRS) MyPosition() (int, error) {
//	return r.stationNumber, nil
//}

// StopAll just stops all relays
func (r *Rev2BoardRS) StopAll() error {
	// Please add here RunCommand with all zero relays
	return errors.New("not implemented")
}

// SendPing just pings the station periodically
func (r *Rev2BoardRS) SendPing() (map[int]bool, error) {
	stations := map[int]bool{}
	for i := 1; i <= app.MAX_ALLOWED_DEVICES; i++ {
		_, err := r.openPort.Write([]byte(fmt.Sprintf("S%d PING;", i)))

		if err != nil {
			continue
		}

		buf := make([]byte, 32)
		N, err := readPort(r.openPort, buf)
		if err != nil {
			continue
		}
		if N < 2 {
			continue
		}
		answer, err := strconv.ParseInt(string(buf[0:2]), 10, 0)
		if err != nil {
			continue
		}
		fmt.Printf("ping ok, post=%d\n", answer)
		stations[int(answer)] = true
	}
	if len(stations) == 0 {
		return stations, app.ErrNotFoundBoard
	}
	return stations, nil
}

// RunConfig just runs a config
func (r *Rev2BoardRS) RunConfig(config app.RelayConfig) {
	r.Commands <- config
}

func (r *Rev2BoardRS) Port() string {
	return r.osPath
}

func readPort(s *serial.Port, buf []byte) (int, error) {
	count := 0
	b := make([]byte, 32)
	t := time.Now()
	read := true
	for read {
		if time.Since(t) > readTimeout {
			return 0, io.EOF
		}
		n, err := s.Read(b)
		if err != nil {
			return 0, err
		}
		for i := 0; i < n; i++ {
			if b[i] == ';' {
				read = false
				n = i + 1
				break
			}
		}
		if count+n > len(buf) {
			return 0, fmt.Errorf("buffer overflow: %d + %d > %d", count, n, len(buf))
		}
		copy(buf[count:], b[:n])
		count += n
	}
	return count, nil
}
