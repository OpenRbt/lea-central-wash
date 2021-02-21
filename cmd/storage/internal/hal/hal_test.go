package hal

import (
	"testing"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

func newRelay(pos int, ontime int, offtime int) app.Relay {
	return app.Relay{
		ID:      pos,
		TimeOn:  ontime,
		TimeOff: offtime,
	}
}
func TestFilesRH(t *testing.T) {
	l, err := NewHardwareAccessLayer()
	if err != nil {
		t.Error("can't create an object")
	}
	hw := l.(*HardwareAccessLayer)
	hw.Start()
	time.Sleep(time.Second * 1)
	board, err := hw.ControlBoard(3)

	cfg := &app.RelayConfig{
		MotorSpeedPercent: 33,
		TimeoutSec:        100,
		Timings:           make([]app.Relay, 0, 12),
	}

	cfg.Timings = append(cfg.Timings, newRelay(1, 500, 500))
	cfg.Timings = append(cfg.Timings, newRelay(3, 500, 0))
	cfg.Timings = append(cfg.Timings, newRelay(4, 0, 0))
	cfg.Timings = append(cfg.Timings, newRelay(5, 0, 500))
	cfg.Timings = append(cfg.Timings, newRelay(6, 1000, 0))
	cfg.Timings = append(cfg.Timings, newRelay(7, 250, 100))

	if err != nil {
		t.Error(err)
		t.Error("No board found")
	} else {
		board.RunConfig(*cfg)
	}
	time.Sleep(time.Second * 28)
	t.Error("Hi")
}
