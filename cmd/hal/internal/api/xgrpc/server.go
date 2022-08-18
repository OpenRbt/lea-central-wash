package xgrpc

import (
	"context"
	"fmt"
	"hal/internal/app"

	emptypb "google.golang.org/protobuf/types/known/emptypb"
)

type HalHandler struct {
	hal app.HardwareAccessLayer
}

func New(hal app.HardwareAccessLayer) *HalHandler {
	return &HalHandler{hal: hal}
}

func (h HalHandler) RunProgram(ctx context.Context, in *Options) (*AnswerProgram, error) {
	err := h.hal.RunProgram(in.ProgramId, app.RelayConfig{
		MotorSpeedPercent: in.MotorSpeedPercent,
		TimeoutSec:        in.TimeoutSec,
	})
	if err != nil {
		return &AnswerProgram{Answer: 1}, err
	}

	return &AnswerProgram{Answer: 1}, nil
}

func (h HalHandler) MeasureVolumeMilliliters(ctx context.Context, in *OptionsCommand) (*AnswerCommand, error) {
	err := h.hal.MeasureVolumeMilliliters(int(in.Command))
	fmt.Println("Server-hal: ", err)
	if err != nil {
		fmt.Println("Not NIL")
		return &AnswerCommand{Answer: 1}, err
	}
	fmt.Println("NIL")
	return &AnswerCommand{Answer: 1}, nil
}

func (h HalHandler) Volume(ctx context.Context, in *emptypb.Empty) (*Answer, error) {
	status := h.hal.Volume()
	if status.IsSensorActive != nil {
		return &Answer{Answer: status.Milliliters, Status: fmt.Sprint(status.IsSensorActive)}, nil
	}
	return &Answer{Answer: status.Milliliters, Status: ""}, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
