package rs485

import (
	"fmt"
	"strings"

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
const (
	FreqGenModelDefaultStr = "default"
	FreqGenModelESQ500Str  = "esq500"
	FreqGenModelAE200HStr  = "ae200h"
	FreqGenModelESQ770Str  = "esq770"
)

func (f FreqGenModel) String() string {
	switch f {
	case FreqGenModelESQ500:
		return FreqGenModelESQ500Str
	case FreqGenModelESQ770:
		return FreqGenModelESQ770Str
	case FreqGenModelAE200H:
		return FreqGenModelAE200HStr
	default:
		return "ERROR_MODEL"
	}
}

// StringToFreqGenModel return model from the string
func StringToFreqGenModel(model string) (FreqGenModel, error) {
	switch strings.ToLower(model) {
	case "ae200h":
		return FreqGenModelAE200H, nil
	case "esq770":
		return FreqGenModelESQ770, nil
	case "esq500":
		return FreqGenModelESQ500, nil
	default:
		return -1, fmt.Errorf("model of frequency generator [%s] does not exist, %w", app.ErrModelDoesNotExist)
	}
}

func MustStringToFreqGenModel(model string) FreqGenModel {
	modelRes, err := StringToFreqGenModel(model)
	if err != nil {
		panic(err)
	}
	return modelRes
}

var modelList []FreqGenModel

func ModelList(defaultModel FreqGenModel) []FreqGenModel {
	if modelList != nil && len(modelList) > 0 && modelList[0] == defaultModel {
		return modelList
	}
	res := make([]FreqGenModel, 0, 10)
	res = append(res, defaultModel)
	if defaultModel != FreqGenModelESQ770 {
		res = append(res, FreqGenModelESQ770)
	}
	if defaultModel != FreqGenModelAE200H {
		res = append(res, FreqGenModelAE200H)
	}
	if defaultModel != FreqGenModelESQ500 {
		res = append(res, FreqGenModelESQ500)
	}
	// no mutex because it's ok if we create the list few times
	modelList = res
	return res
}

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
