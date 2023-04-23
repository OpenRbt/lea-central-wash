package requester

import (
	"context"
	"fmt"
	"sync"
	"testing"
	"time"
)

func TestSequences(t *testing.T) {
	motor := NewDummyMotor(5000, 6000)
	sequencer := NewSequenceRequester(context.Background(), motor)
	go func() {
		for {
			res := sequencer.loop()
			fmt.Printf("executed priority task %d\n", res)
		}
	}()
	a := sequencer.HighPrioritySetSpeedPercent(1, 10)
	for a.Err != nil {
		t.Errorf("can't set motor speed %+v", a.Err)
		a = sequencer.HighPrioritySetSpeedPercent(1, 10)
	}

	a = sequencer.HighPriorityGetSpeedPercent(1)
	for a.Err != nil {
		t.Errorf("can't get motor speed %+v", a.Err)
		a = sequencer.HighPriorityGetSpeedPercent(1)
	}

	if a.Val < 9.99 || a.Val > 10.01 {
		t.Errorf("percent must be 10 but its %f", a.Val)
	}
}

func TestSequencesPriority(t *testing.T) {
	motor := NewDummyMotor(5000, 6000)
	sequencer := NewSequenceRequester(context.Background(), motor)

	go func() {
		time.Sleep(time.Millisecond * 100)
		for {
			res := sequencer.loop()
			fmt.Printf("executed priority task %d\n", res)
		}
	}()

	var wg sync.WaitGroup
	wg.Add(1)
	wg.Add(1)
	wg.Add(1)
	result := 0
	var resultMutex sync.Mutex

	go func() {
		for k := 0; k < 20; k++ {
			a := sequencer.HighPrioritySetSpeedPercent(1, 10)
			for a.Err != nil {
				a = sequencer.HighPrioritySetSpeedPercent(1, 10)
			}
		}
		resultMutex.Lock()
		if result == 0 {
			result = 13
		}
		resultMutex.Unlock()
		wg.Done()
	}()

	go func() {
		for k := 0; k < 20; k++ {
			a := sequencer.HighPrioritySetSpeedPercent(1, 10)
			for a.Err != nil {
				a = sequencer.HighPrioritySetSpeedPercent(1, 10)
			}
		}
		resultMutex.Lock()
		if result == 0 {
			result = 13
		}
		resultMutex.Unlock()
		wg.Done()
	}()

	go func() {
		for k := 0; k < 10; k++ {
			a := sequencer.LowPriorityGetSpeedPercent(1)
			for a.Err != nil {
				a = sequencer.LowPriorityGetSpeedPercent(1)
			}
		}
		resultMutex.Lock()
		if result == 0 {
			result = 12
		}
		resultMutex.Unlock()
		wg.Done()
	}()

	wg.Wait()
	if result != 13 {
		t.Errorf("High Priorities must go first, %d", result)
	}
}
