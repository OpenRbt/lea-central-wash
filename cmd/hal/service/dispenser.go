package service

import (
	"fmt"
	"strconv"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"
	"github.com/tarm/serial"
)

const dispenserAnswer = "YF-S201;"

type Rev1DispencerBoard struct {
	resourcesMu           sync.Mutex
	osPath                string
	openPort              *serial.Port
	toRemove              bool
	errorCount            int
	lastVolume            int64
	allowedPing           bool
	stopDispenser         bool
	ErrorCommandDispenser error
	commandStopRev2Board  app.RelayConfig
	commandStartRev2Board app.RelayConfig
	timeoutSec            int
	runOnRev2Board        func(int32, app.RelayConfig) error
	hwMetrics             app.HardwareMetrics
}

// NewDispencerBoard is a constructor
func NewDispencerBoard(osPath string, openPort *serial.Port, runOnRev2Board func(int32, app.RelayConfig) error, hwMetrics app.HardwareMetrics) *Rev1DispencerBoard {
	return &Rev1DispencerBoard{
		osPath:                osPath,
		openPort:              openPort,
		toRemove:              false,
		allowedPing:           true,
		stopDispenser:         false,
		ErrorCommandDispenser: nil,
		timeoutSec:            3,
		runOnRev2Board:        runOnRev2Board,
		hwMetrics:             hwMetrics,
	}
}

func (r *Rev1DispencerBoard) SetStopDispenser(t bool) {
	r.resourcesMu.Lock()
	r.stopDispenser = t
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetStopDispenser() bool {
	r.resourcesMu.Lock()
	t := r.stopDispenser
	r.resourcesMu.Unlock()
	return t
}

func (r *Rev1DispencerBoard) SetLastVolume(volume int64) {
	r.resourcesMu.Lock()
	r.lastVolume = volume
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetLastVolume() int64 {
	r.resourcesMu.Lock()
	volume := r.lastVolume
	r.resourcesMu.Unlock()
	return volume
}

func (r *Rev1DispencerBoard) SetAllowedPing(t bool) {
	r.resourcesMu.Lock()
	r.allowedPing = t
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetAllowedPing() bool {
	r.resourcesMu.Lock()
	t := r.allowedPing
	r.resourcesMu.Unlock()
	return t
}

func (r *Rev1DispencerBoard) SetErrComandDispenser(err error) {
	r.resourcesMu.Lock()
	r.ErrorCommandDispenser = err
	r.resourcesMu.Unlock()
}

func (r *Rev1DispencerBoard) GetErrComandDispenser() error {
	r.resourcesMu.Lock()
	err := r.ErrorCommandDispenser
	r.resourcesMu.Unlock()
	return err
}

func (r *Rev1DispencerBoard) RunCommandStartRev2Board() error {
	r.resourcesMu.Lock()
	relay := r.commandStartRev2Board
	r.resourcesMu.Unlock()
	return r.runOnRev2Board(1, relay)
}

func (r *Rev1DispencerBoard) RunCommandStopRev2Board() error {
	r.resourcesMu.Lock()
	relay := r.commandStopRev2Board
	r.resourcesMu.Unlock()
	return r.runOnRev2Board(1, relay)
}

func (r *Rev1DispencerBoard) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev1DispencerBoard) workingLoop() {
	duration := 800 * time.Millisecond
	timer := time.NewTimer(duration)
	for {
		<-timer.C
		timer.Stop()
		if r.GetAllowedPing() {
			err := r.SendPing()
			if err != nil {
				r.resourcesMu.Lock()
				r.errorCount++
				errCount := r.errorCount
				r.resourcesMu.Unlock()
				if errCount >= 5 {
					fmt.Println("Delete Rev1DispenserBoard")
					r.resourcesMu.Lock()
					r.toRemove = true
					r.resourcesMu.Unlock()
					return
				}
			} else {
				r.resourcesMu.Lock()
				r.errorCount = 0
				r.resourcesMu.Unlock()
			}
		} else {
			r.SetAllowedPing(true)
		}
		timer.Reset(duration)
	}
}

func (r *Rev1DispencerBoard) volume() app.DispenserStatus {
	return app.DispenserStatus{
		Milliliters:           int64(r.GetLastVolume()),
		ErrorCommandDispenser: r.GetErrComandDispenser(),
	}
}

func (r *Rev1DispencerBoard) dispenserStop() error {
	r.SetStopDispenser(true)
	return nil
}

func (r *Rev1DispencerBoard) reconnect() error {
	r.hwMetrics.Rev1DispencerReconnectCounter.Inc("1")
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

func (r *Rev1DispencerBoard) measureVolumeMilliliters(measureVolume int) error {
	r.SetAllowedPing(false)
	r.SetLastVolume(0)
	r.SetErrComandDispenser(nil)
	countErrRead := 0
	stopDispenser := 0
	startFluid := 20
	buf := make([]byte, 32)
	cmdd := "S" + strconv.Itoa(measureVolume) + ";"
	exi := false
	fmt.Println("In measure")
	for i := 0; i < 10; i++ {
		_, err := r.openPort.Write([]byte(cmdd))
		if err != nil {
			fmt.Println("Error in command ", measureVolume)
			r.SetErrComandDispenser(err)
		} else {
			N, err := r.openPort.Read(buf)
			if err == nil && N > 1 {
				ans := string(buf[0 : N-2])
				if ans == "SOK;" {
					fmt.Println("Start command ", measureVolume)
					r.SetErrComandDispenser(nil)
					exi = true
					break
				}
			}
		}
	}
	if exi {
		tick := time.Tick(200 * time.Millisecond)
		var countErr int = 0
		for {
			select {
			case <-tick:
				if startFluid > 5 {
					err := r.RunCommandStartRev2Board()
					if err != nil {
						fmt.Printf("error RunCommandStartRev2Board: %s\n", err)
					} else {
						startFluid = 0
					}
				}
				startFluid++
				r.SetAllowedPing(false)
				if r.GetStopDispenser() {
					if stopDispenser > 0 {
						_, err := r.openPort.Write([]byte("FOK;"))
						if err != nil {
							fmt.Println("Error in command FOK")
							r.SetErrComandDispenser(app.ErrInCommandFok)
							return app.ErrInCommandFok
						}
						return nil
					}
					err := r.RunCommandStopRev2Board()
					if err != nil {
						fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
					} else {
						stopDispenser++
					}
				}
				N, err := r.openPort.Read(buf)
				if err == nil && N > 2 {
					countErrRead = 0
					ans := string(buf[0 : N-2])
					v, err := strconv.ParseInt(ans[1:N-3], 10, 64)
					if err != nil {
						v = r.GetLastVolume()
						fmt.Println(err)
					}
					fmt.Printf("Current volume: %d\n", v)
					if v-r.GetLastVolume() < 1 {
						countErr += 1
						if countErr >= 50 {
							err := r.RunCommandStopRev2Board()
							if err != nil {
								fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
							}
							_, err = r.openPort.Write([]byte("ERR;"))
							N, err = r.openPort.Read(buf)
							ans = string(buf[0 : N-2])
							if ans == "FOK;" {
								r.SetErrComandDispenser(app.ErrNonFreezing)
								fmt.Println("The non-freezing is over")
								return app.ErrNonFreezing
							}
						}
					} else {
						countErr = 0
						r.SetLastVolume(v)
					}
					if ans[0] == 'F' || v >= int64(measureVolume) {
						fmt.Println("Finish command ", measureVolume, " Successfully!")
						err := r.RunCommandStopRev2Board()
						if err != nil {
							fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
						}
						countErr = 0
						r.SetLastVolume(v)
						_, err = r.openPort.Write([]byte("FOK;"))
						if err != nil {
							r.SetErrComandDispenser(app.ErrInCommandFok)
							fmt.Println("Error in command FOK")
							return app.ErrInCommandFok
						}
						return nil
					}
				} else {
					countErrRead += 1
					fmt.Println("Error read answer dispenser")
					if countErrRead > 2 {
						r.reconnect()
					}
					if countErrRead > 5 {
						err := r.RunCommandStopRev2Board()
						if err != nil {
							fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
						} else {
							stopDispenser++
						}
						_, _ = r.openPort.Write([]byte("FOK;"))
						r.SetErrComandDispenser(app.ErrReadAnswerDispenser)
						fmt.Println("Error in command ", measureVolume)
						return err
					}
				}
			}
		}
	}
	r.SetErrComandDispenser(app.ErrDispenserNotRespond)
	fmt.Println("Dispenser is not responding")
	err := r.RunCommandStopRev2Board()
	if err != nil {
		fmt.Printf("error RunCommandStopRev2Board: %s\n", err)
	}
	return app.ErrDispenserNotRespond
}

func (r *Rev1DispencerBoard) SendPing() error {
	_, err := r.openPort.Write([]byte("PING;"))
	if err != nil {
		return err
	}

	buf := make([]byte, 32)
	_, err = r.openPort.Read(buf)
	if err != nil {
		return err
	}
	st := string(buf[0:8])
	if st != "OK-PING;" {
		return err
	}
	fmt.Println("Ping-OK")
	return nil
}

func (r *Rev1DispencerBoard) getLevel() int {
	cmd := "L;"
	_, err := r.openPort.Write([]byte(cmd))
	if err != nil {
		fmt.Println("Error in command", cmd)
		return -1
	}
	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
		fmt.Println("Error in read answer")
		return -1
	}
	ans := string(buf[0 : N-2])
	n, err := strconv.Atoi(ans)
	if err != nil {
		fmt.Println("Error in convert string in int")
		return -1
	}
	return n
}
