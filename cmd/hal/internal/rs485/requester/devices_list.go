package requester

import "github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/app"

type DevicesList struct {
	lastRes   int8
	devExists [app.MAX_ALLOWED_DEVICES + 1]bool
}

func NewDeviceList() *DevicesList {
}

func (d *DevicesList) Count() int8 {
	res := int8(0)
	for i := 0; i <= app.MAX_ALLOWED_DEVICES; i++ {
		if d.devExists[i] {
			res++
		}
	}
	return res
}
