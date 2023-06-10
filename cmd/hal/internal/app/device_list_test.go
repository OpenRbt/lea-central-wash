package app

import "testing"

func Test1(t *testing.T) {
	devList := NewDeviceList()
	devList.AddDevice(1)
	devList2 := NewDeviceList()
	devList2.AddDevice(3)
	devList2.AddDevice(4)
	devList.AddRange(devList2)

	expectedList := NewDeviceListM(1, 3, 4)
	if !devList.IsEqual(expectedList) {
		t.Errorf("lists must be equal")
	}
}

func TestMaxAndMin(t *testing.T) {
	devList := NewDeviceList()
	err := devList.AddDevice(MAX_ALLOWED_DEVICES)
	if err != nil {
		t.Errorf("cant add device %+v", err)
	}
	devList2 := NewDeviceListM(MAX_ALLOWED_DEVICES)
	if !devList.IsEqual(devList2) {
		t.Errorf("lists must be equal")
	}
}

func TestDel(t *testing.T) {
	devList := NewDeviceListM(1, 2, 3)
	expectedList := NewDeviceListM(1, 3)
	if devList.IsEqual(expectedList) {
		t.Errorf("lists must NOT be equal")
	}
	devList.DelDevice(2)
	if !devList.IsEqual(expectedList) {
		t.Errorf("lists must be equal")
	}
}
