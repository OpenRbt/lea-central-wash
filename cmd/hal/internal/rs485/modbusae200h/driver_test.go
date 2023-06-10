//go:build integration

package modbusae200h

import (
	"testing"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/rsutil"
)

func TestClient(t *testing.T) {
	name := "tty.usbserial-10"
	cl, err := NewFrequencyGenerator(rsutil.NewRS485Config(name, 19200, 10000))
	if err != nil {
		t.Error(err)
		return
	}

	err = cl.ReadData()
	if err != nil {
		t.Errorf("cant read %+v", err)

	}
	t.Error("STOP")
}
