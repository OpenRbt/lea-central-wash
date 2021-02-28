package hal

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/tarm/serial"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

const uidAnswerRegex = "UID\\s([0-9A-F]*);"

// Errors ...
var (
	ErrWrongAnswer = errors.New("wrong answer from the board")
)

// PostError describes an error happened to a post
type PostError struct {
	PostNumber    int
	FailedCommand app.RelayConfig
}

// HardwareAccessLayer is the whole layer to communicate with boards
type HardwareAccessLayer struct {
	uidAnswer *regexp.Regexp
	portsMu   sync.Mutex
	ports     map[string]*Rev2Board
	Errors    chan PostError
}

// Rev2Board describes Revision 2 openrbt.com board
type Rev2Board struct {
	osPath        string
	openPort      *serial.Port
	toRemove      bool
	stationNumber int
	errorCount    int
	Commands      chan app.RelayConfig
}

// NewRev2Board is a constructor
func NewRev2Board(osPath string, openPort *serial.Port) *Rev2Board {
	return &Rev2Board{
		osPath:        osPath,
		openPort:      openPort,
		stationNumber: -1,
		toRemove:      false,
		Commands:      make(chan app.RelayConfig),
	}
}

// CollectAvailableSerialPorts reads /dev directory and find all ttyUSB* devices
func (h *HardwareAccessLayer) CollectAvailableSerialPorts() {
	h.uidAnswer, _ = regexp.Compile(uidAnswerRegex)
	files, err := ioutil.ReadDir("/dev")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("listing files...")
	for _, f := range files {
		if strings.HasPrefix(f.Name(), "ttyUSB") {
			_, err = h.findPort(f.Name())
			if err != nil {
				// port is not found in our dictionary
				h.checkAndAddPort(f.Name())
			} else {
				fmt.Printf("Port is already added: %s, no action required\n", f.Name())
			}
			fmt.Println(f.Name())
		}
	}
}

// Run just runs a goroutine which controls a device
func (r *Rev2Board) Run() error {
	go r.workingLoop()
	return nil
}

func (r *Rev2Board) workingLoop() {
	tick := time.Tick(800 * time.Millisecond)
	var cmd app.RelayConfig
	for {
		select {
		case <-tick:
			stationNumber, err := r.SendPing()
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
				err = r.runCommand(cmd)
			}
		}
	}
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
		cmdBuf.WriteString(strconv.Itoa(cmd.TimeoutSec))
		cmdBuf.WriteString("|")
	}
	if cmd.MotorSpeedPercent >= 0 {
		cmdBuf.WriteString("M")
		cmdBuf.WriteString(strconv.Itoa(cmd.MotorSpeedPercent))
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
		return err
	}

	buf := make([]byte, 32)
	N, err := r.openPort.Read(buf)
	if err != nil {
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

func (h *HardwareAccessLayer) findPort(key string) (*Rev2Board, error) {
	h.portsMu.Lock()
	el, found := h.ports[key]
	h.portsMu.Unlock()
	if !found {
		return nil, app.ErrNotFound
	}
	return el, nil
}

func (h *HardwareAccessLayer) deletePort(key string) {
	h.portsMu.Lock()
	delete(h.ports, key)
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) addPort(key string, board *Rev2Board) {
	h.portsMu.Lock()
	h.ports[key] = board
	h.portsMu.Unlock()
}

func (h *HardwareAccessLayer) checkAndAddPort(key string) error {
	c := &serial.Config{Name: "/dev/" + key, Baud: 38400, ReadTimeout: time.Millisecond * 100}
	s, err := serial.OpenPort(c)
	if err != nil {
		return err
	}

	_, err = s.Write([]byte("UID;"))
	if err != nil {
		return err
	}

	buf := make([]byte, 128)
	N, err := s.Read(buf)
	if err != nil {
		s.Close()
		return err
	}
	if N < 3 {
		s.Close()
		return nil
	}
	ans := string(buf[0:N])
	fmt.Printf("answer is [%s]\n", ans)
	foundStrings := h.uidAnswer.FindStringSubmatch(ans)
	if len(foundStrings) < 2 {
		s.Close()
		return nil
	}

	fmt.Printf("uid is [%s]\n", foundStrings[1])
	board := NewRev2Board(key, s)
	h.addPort(key, board)
	return board.Run()
}

// RunConfig just runs a config
func (r *Rev2Board) RunConfig(config app.RelayConfig) {
	r.Commands <- config
}

// ControlBoard returns required control board by its key
func (h *HardwareAccessLayer) ControlBoard(wantedPosition int) (app.ControlBoard, error) {
	h.portsMu.Lock()
	for key := range h.ports {
		if h.ports[key].stationNumber == wantedPosition {
			return h.ports[key], nil
		}
	}
	defer h.portsMu.Unlock()
	return nil, app.ErrNotFound
}

// Start just starts everything
func (h *HardwareAccessLayer) Start() {
	go h.workingLoop()
}

func (h *HardwareAccessLayer) workingLoop() ([]app.ControlBoard, error) {
	for {
		h.CollectAvailableSerialPorts()
		t := true
		h.portsMu.Lock()
		for t {
			t = false
			for key := range h.ports {
				if h.ports[key].toRemove {
					delete(h.ports, key)
					t = true
					break
				}
			}
		}
		h.portsMu.Unlock()
		time.Sleep(time.Second) // Let's do maintenance just once per second
	}
}

// NewHardwareAccessLayer is just a constructor
func NewHardwareAccessLayer() (app.HardwareAccessLayer, error) {
	res := &HardwareAccessLayer{
		ports: make(map[string]*Rev2Board),
	}
	return res, nil
}

// RunProgram
func (h *HardwareAccessLayer) RunProgram(id int, config app.RelayConfig) (err error) {
	board, err := h.ControlBoard(id)
	if err != nil {
		return err
	}
	board.RunConfig(config)
	return nil
}
