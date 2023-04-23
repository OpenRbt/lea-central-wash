package rs485

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/modbusae200h"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/rs485/requester"
)

func TestMotorManagerWorking(t *testing.T) {
	motor := requester.NewDummyMotor(5000, 6000)
	ctx, cancel := context.WithCancel(context.Background())
	portReporter := &requester.DummyPortReporter{}
	manager := NewMotorManager(ctx, portReporter, time.Millisecond*300)
	sequenceRequester := requester.NewSequenceRequester(ctx, motor)
	manager.AddSequenceRequester(sequenceRequester)
	err := manager.Run()
	if err != nil {
		fmt.Println("manager already running...")
	}
	time.Sleep(100 * time.Millisecond)
	err = manager.SetSpeedPercent(1, 50)
	if err != nil {
		t.Error(err)
	}
	err = manager.SetSpeedPercent(10, 50)
	if err == nil {
		t.Error("motor 10 must not be available")
	} else {
		fmt.Printf("motor 10 failed as expected %+v\n", err)
	}
	motor.Kill()
	time.Sleep(500 * time.Millisecond)
	err = manager.SetSpeedPercent(1, 50)
	if err == nil {
		t.Error(err)
	} else {
		fmt.Printf("err as expected %+v\n", err)
	}

	cancel()
	time.Sleep(500 * time.Millisecond)
}

func TestMotorManagerWorkingWithCancel(t *testing.T) {
	motor := requester.NewDummyMotor(5000, 6000)
	ctx, cancel := context.WithCancel(context.Background())
	portReporter := &requester.DummyPortReporter{}
	manager := NewMotorManager(ctx, portReporter, time.Millisecond*300)
	sequenceRequester := requester.NewSequenceRequester(ctx, motor)
	manager.AddSequenceRequester(sequenceRequester)
	err := manager.Run()
	if err != nil {
		fmt.Println("manager already running...")
	}
	time.Sleep(100 * time.Millisecond)
	err = manager.SetSpeedPercent(1, 50)
	if err != nil {
		t.Error(err)
	}
	err = manager.SetSpeedPercent(10, 50)
	if err == nil {
		t.Error("motor 10 must not be available")
	} else {
		fmt.Printf("motor 10 failed as expected %+v\n", err)
	}
	cancel()
	time.Sleep(500 * time.Millisecond)
}

func TestMotorManagerReal(t *testing.T) {
	motor, err := modbusae200h.NewFrequencyGenerator("tty.usbserial-10", 9600, 5000)
	if err != nil {
		t.Errorf("cant create motor %v", err)
		return
	}
	ctx, cancel := context.WithCancel(context.Background())
	portReporter := &requester.DummyPortReporter{}
	manager := NewMotorManager(ctx, portReporter, time.Millisecond*300)
	sequenceRequester := requester.NewSequenceRequester(ctx, motor)
	manager.AddSequenceRequester(sequenceRequester)
	devices := manager.collectInfoIteration()
	fmt.Printf("found %d devices\n", devices)
	err = manager.Run()
	if err != nil {
		fmt.Println("manager already running...")
	}
	time.Sleep(100 * time.Millisecond)
	err = manager.SetSpeedPercent(1, 50)
	if err != nil {
		t.Error(err)
	}
	err = manager.SetSpeedPercent(2, 60)
	if err != nil {
		t.Error(err)
	}
	err = manager.SetSpeedPercent(10, 50)
	if err == nil {
		t.Error("motor 10 must not be available")
	} else {
		fmt.Printf("motor 10 failed as expected %+v\n", err)
	}
	cancel()
	time.Sleep(500 * time.Millisecond)
}
