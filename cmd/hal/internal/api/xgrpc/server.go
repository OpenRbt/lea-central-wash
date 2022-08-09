package xgrpc

import (
	"context"
	"hal/internal/app"

	"github.com/golang/protobuf/ptypes/empty"
	emptypb "google.golang.org/protobuf/types/known/emptypb"
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

func (h HalHandler) Command(ctx context.Context, in *Opti) (*empty.Empty, error) {
	err := h.hal.Command(int(in.Cmd))
	if err != nil {
		return nil, err
	}

	return nil, nil
}

func (h HalHandler) Volume(ctx context.Context, in *emptypb.Empty) (*Answer, error) {
	ans := h.hal.Volume()
	return &Answer{Ans: ans}, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
