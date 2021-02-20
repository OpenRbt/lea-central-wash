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

// HardwareAccessLayer is the whole layer to communicate with boards
type HardwareAccessLayer struct {
	uidAnswer *regexp.Regexp
	portsMu   sync.Mutex
	ports     map[string]*Rev2Board
}

// Rev2Board describes Revision 2 openrbt.com board
type Rev2Board struct {
	osPath        string
	openPort      *serial.Port
	toRemove      bool
	stationNumber int
	errorCount    int
}

// NewRev2Board is a constructor
func NewRev2Board(osPath string, openPort *serial.Port) *Rev2Board {
	return &Rev2Board{
		osPath:        osPath,
		openPort:      openPort,
		stationNumber: -1,
		toRemove:      false,
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

func (r *Rev2Board) workingLoop() error {
	for {
		stationNumber, err := r.SendPing()
		if err != nil {
			r.errorCount++
			continue
		}
		r.errorCount = 0
		r.stationNumber = stationNumber
		time.Sleep(time.Millisecond * 500)
	}
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

// ControlBoard returns required control board by its key
func (h *HardwareAccessLayer) ControlBoard(key int) (app.ControlBoard, error) {
	return nil, errors.New("not implemented")
}

// ControlBoards returns All ControlBoards available (not sure why)
func (h *HardwareAccessLayer) ControlBoards() ([]app.ControlBoard, error) {
	return nil, errors.New("not implemented")
}

// NewHardwareAccessLayer is just a constructor
func NewHardwareAccessLayer() (app.HardwareAccessLayer, error) {
	res := &HardwareAccessLayer{
		ports: make(map[string]*Rev2Board),
	}
	return res, nil
}
