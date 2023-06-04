package esq500

import (
	"errors"
	"fmt"
	"math"
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/rsutil"
	"github.com/simonvetter/modbus"
)

type FrequencyGenerator struct {
	portName     string
	client       *modbus.ModbusClient
	modbusMutex  sync.Mutex
	nominalSpeed int
	coolTime     time.Duration
}

func (f *FrequencyGenerator) Port() string {
	return f.portName

}
func (f *FrequencyGenerator) Destroy() error {
	err := f.client.Close()
	return err
}

func NewFrequencyGenerator(cfg rsutil.RS485Config) (*FrequencyGenerator, error) {
	client, err := modbus.NewClient(&modbus.ClientConfiguration{
		URL:      "rtu:///dev/" + cfg.PortName,
		Speed:    cfg.CommunicationSpeed, // default
		DataBits: 8,                      // default, optional
		Parity:   modbus.PARITY_NONE,     // default, optional
		StopBits: 2,                      // default if no parity, optional
		Timeout:  200 * time.Millisecond,
	})

	if err != nil {
		return nil, err
	}
	res := &FrequencyGenerator{
		portName:     cfg.PortName,
		client:       client,
		nominalSpeed: cfg.DefaultMotorSpeed,
		coolTime:     10 * time.Millisecond,
	}

	err = client.Open()
	if err != nil {
		return nil, err
	}
	return res, nil
}

type Ans struct {
	Addr uint16
	Val  []byte
}

func (f *FrequencyGenerator) MaxSpeed(deviceID uint8) (uint16, error) {
	addr := uint16(0x1e02)
	return f.Read16bit(deviceID, addr)
}

func (f *FrequencyGenerator) Read16bit(deviceID uint8, addr uint16) (uint16, error) {
	f.modbusMutex.Lock()
	defer f.modbusMutex.Unlock()
	f.client.SetUnitId(deviceID)
	time.Sleep(f.coolTime)
	val, err := f.client.ReadBytes(addr, 2, modbus.HOLDING_REGISTER)

	if err != nil {
		return 0, err
	}

	res1 := uint16(val[0]) * 256
	res2 := uint16(val[1])
	return res1 + res2, nil
}

func (f *FrequencyGenerator) SetSpeedPercent(deviceID uint8, percent int16) error {
	if percent > 200 {
		percent = 200
	}
	if percent < 0 {
		percent = 0
	}
	var realRPM float64
	realRPM = float64(f.nominalSpeed)
	realRPM *= float64(percent)
	realRPM /= float64(100)

	if realRPM < 0 {
		realRPM = -realRPM
	}
	if realRPM > 65000 {
		realRPM = 65000
	}

	realValue := uint16(math.Round(realRPM))

	f.modbusMutex.Lock()
	defer f.modbusMutex.Unlock()
	f.client.SetUnitId(deviceID)

	LowVal := realValue % 256
	HighVal := realValue / 256

	fmt.Printf("final value %d percent, low %d, high %d\n", percent, LowVal, HighVal)
	time.Sleep(f.coolTime)
	err := f.client.WriteRegister(0x1e01, realValue)
	if err == modbus.ErrRequestTimedOut {
		fmt.Printf("%s: id:%d, driver: cant set speed %+v\n", f.portName, deviceID, err)
		return err
	}
	return nil
}
func (f *FrequencyGenerator) GetSpeedPercent(deviceID uint8) (int16, error) {
	f.modbusMutex.Lock()
	defer f.modbusMutex.Unlock()
	ErrNominalSpeed := errors.New("zero nominal speed")
	if f.nominalSpeed == 0 {
		return 0, fmt.Errorf("can't divide to zero nominal speed %+w", ErrNominalSpeed)
	}
	time.Sleep(f.coolTime)
	res, err := f.Read16bit(deviceID, 0x1e01)

	if err != nil {
		return 0, err
	}
	percentValue := float64(res)
	percentValue *= float64(100)
	percentValue /= float64(f.nominalSpeed)
	return int16(percentValue), nil
}

func (f *FrequencyGenerator) Temperature(device uint8) (float32, error) {
	return 50.0, nil
}

func (f *FrequencyGenerator) StartMotor(deviceID uint8) error {
	f.modbusMutex.Lock()
	defer f.modbusMutex.Unlock()
	addr := uint16(0x1e00)
	f.client.SetUnitId(deviceID)
	time.Sleep(f.coolTime)
	err := f.client.WriteRegister(addr, 0x5)

	if err == modbus.ErrRequestTimedOut {
		fmt.Printf("%s: id:%d, driver: cant start %+v\n", f.portName, deviceID, err)
		return err
	}

	return nil
}

func (f *FrequencyGenerator) StopMotor(deviceID uint8) error {
	f.modbusMutex.Lock()
	defer f.modbusMutex.Unlock()
	f.client.SetUnitId(deviceID)
	addr := uint16(0x1e00)
	time.Sleep(f.coolTime)
	err := f.client.WriteRegister(addr, 0x6)
	if err == modbus.ErrRequestTimedOut {
		fmt.Printf("%s: id:%d, driver: cant stop %+v\n", f.portName, deviceID, err)
		return err
	}
	return nil
}
