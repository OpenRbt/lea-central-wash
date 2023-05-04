//go:build integration

package modbusae200h

import "testing"

func TestClient(t *testing.T) {
	name := "tty.usbserial-10"
	cl, err := NewFrequencyGenerator(name, 9600, 5000)
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
