package xgrpc

import (
	"context"
	"github.com/golang/protobuf/ptypes/empty"
	"hal/internal/app"
)

type HalHandler struct {
	hal app.HardwareAccessLayer
}

func New(hal app.HardwareAccessLayer) *HalHandler {
	return &HalHandler{hal: hal}
}

func (h HalHandler) RunProgram(ctx context.Context, in *Options) (*empty.Empty, error) {
	err := h.hal.RunProgram(in.ProgramId, app.RelayConfig{
		MotorSpeedPercent: in.MotorSpeedPercent,
		TimeoutSec:        in.TimeoutSec,
	})
	if err != nil {
		return nil, err
	}

	return nil, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
