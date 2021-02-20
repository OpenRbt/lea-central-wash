package hal

import (
	"testing"
	"time"
)

func TestFilesRH(t *testing.T) {
	l, err := NewHardwareAccessLayer()
	if err != nil {
		t.Error("can't create an object")
	}
	hw := l.(*HardwareAccessLayer)
	hw.CollectAvailableSerialPorts()
	time.Sleep(time.Second * 20)
	t.Error("Hi")
}
