package app

import (
	"fmt"
	"sync"
)

type DevicesList struct {
	lastCount   int8
	devExists   [MAX_ALLOWED_DEVICES + 1]bool
	globalMutex sync.RWMutex
}

// IsEqual is for testing purposes now, try to fix it for concurrency for the real project usage
func (d *DevicesList) IsEqual(anotherList *DevicesList) bool {
	return anotherList.devExists == d.devExists
}

func (d *DevicesList) AddDevice(deviceId int8) error {
	if deviceId <= 0 || deviceId > MAX_ALLOWED_DEVICES {
		return fmt.Errorf("wrong device index %d, must be between 1 and %d, %w", deviceId, MAX_ALLOWED_DEVICES, ErrWrongIndex)
	}
	d.globalMutex.Lock()
	if !d.devExists[deviceId] {
		d.devExists[deviceId] = true
		d.lastCount++
	}
	d.globalMutex.Unlock()
	return nil
}

func (d *DevicesList) AddRange(anotherList *DevicesList) {
	for i := int8(0); i <= MAX_ALLOWED_DEVICES; i++ {
		if anotherList.Contains(i) {
			err := d.AddDevice(i)
			if err != nil {
				panic(err) // it means we have an error in the source code
			}
		}
	}
}

func (d *DevicesList) Clean() {
	d.globalMutex.Lock()
	for i := 0; i <= MAX_ALLOWED_DEVICES; i++ {
		d.devExists[i] = false
	}
	d.lastCount = 0
	d.globalMutex.Unlock()
}

func (d *DevicesList) Contains(deviceID int8) bool {
	if deviceID <= 0 || deviceID > MAX_ALLOWED_DEVICES {
		return false
	}
	d.globalMutex.RLock()
	res := d.devExists[deviceID]
	d.globalMutex.RUnlock()
	return res
}

func (d *DevicesList) DelDevice(deviceId int8) error {
	if deviceId <= 0 || deviceId > MAX_ALLOWED_DEVICES {
		return fmt.Errorf("wrong device index %d, must be between 1 and %d, %w", deviceId, MAX_ALLOWED_DEVICES, ErrWrongIndex)
	}
	d.globalMutex.Lock()
	if d.devExists[deviceId] {
		d.devExists[deviceId] = false
		d.lastCount--
	}
	d.globalMutex.Unlock()
	return nil
}

func NewDeviceList() *DevicesList {
	res := &DevicesList{
		lastCount: 0,
	}
	return res
}

func NewDeviceListM(initialVals ...int8) *DevicesList {
	res := NewDeviceList()
	for _, k := range initialVals {
		res.AddDevice(k)
	}
	return res
}

func (d *DevicesList) Count() int8 {
	return d.lastCount
}
