package rs485

import (
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485/esq500"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485/esq770"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485/modbusae200h"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485/requester"
	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/rs485/rsutil"
)

type FreqGenModel int

const (
	FreqGenModelESQ500 FreqGenModel = 1
	FreqGenModelAE200H FreqGenModel = 2
	FreqGenModelESQ770 FreqGenModel = 3
)

func CreateFrequencyGenerator(fgModel FreqGenModel, cfg rsutil.RS485Config) (requester.MotorDriver, error) {
	switch fgModel {
	case FreqGenModelAE200H:
		return modbusae200h.NewFrequencyGenerator(cfg)
	case FreqGenModelESQ500:
		return esq500.NewFrequencyGenerator(cfg)
	case FreqGenModelESQ770:
		return esq770.NewFrequencyGenerator(cfg)
	default:
		return nil, app.ErrModelDoesNotExist
	}
}
