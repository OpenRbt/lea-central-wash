package esq500

import (
	"fmt"
	"testing"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/rsutil"
)

func TestClient(t *testing.T) {
	motorid := uint8(4)
	name := "ttyUSB0"
	cl, err := NewFrequencyGenerator(rsutil.NewRS485Config(name, 9600, 10000))
	if err != nil {
		t.Error(err)
		return
	}

	v, err := cl.MaxSpeed(motorid)
	if err != nil {
		t.Errorf("cant read status %+v", err)
	}
	fmt.Printf("val: %v\n", v)

	v16, err := cl.GetSpeedPercent(motorid)
	if err != nil {
		t.Errorf("cant read speed %+v", err)
	}
	fmt.Printf("val: %v\n", v16)

	err = cl.StartMotor(motorid)
	if err != nil {
		t.Errorf("cant start %+v", err)
	}

	err = cl.SetSpeedPercent(motorid, 50)
	if err != nil {
		t.Errorf("cant set speed %+v", err)
	}
	time.Sleep(10 * time.Second)

	err = cl.SetSpeedPercent(motorid, 0)
	if err != nil {
		t.Errorf("cant set zero speed %+v", err)
	}

	err = cl.StopMotor(motorid)
	if err != nil {
		t.Errorf("cant stop speed %+v", err)
	}

	t.Error("STOP")
}
