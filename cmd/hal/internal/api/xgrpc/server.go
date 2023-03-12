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
	relay := []app.Relay{}
	for _, entry := range in.Relays {
		z := app.Relay{
			ID:      int(entry.ID),
			TimeOn:  int(entry.TimeOn),
			TimeOff: int(entry.TimeOff),
		}
		relay = append(relay, z)
	}

	err := h.hal.RunProgram(in.StationId, app.RelayConfig{
		MotorSpeedPercent: in.MotorSpeedPercent,
		TimeoutSec:        in.TimeoutSec,
		Timings:           relay,
	})
	if err != nil {
		return &AnswerProgram{Answer: 1}, err
	}

	return &AnswerProgram{Answer: 1}, nil
}

func (h HalHandler) MeasureVolumeMilliliters(ctx context.Context, in *OptionsCommand) (*AnswerCommand, error) {
	err := h.hal.MeasureVolumeMilliliters(int(in.Command))
	if err != nil {
		return &AnswerCommand{Answer: 1}, err
	}
	return &AnswerCommand{Answer: 1}, nil
}

func (h HalHandler) Volume(ctx context.Context, in *emptypb.Empty) (*Answer, error) {
	status := h.hal.Volume()
	if status.ErrorCommandDispenser != nil {
		return &Answer{Answer: status.Milliliters, Status: fmt.Sprint(status.ErrorCommandDispenser)}, nil
	}
	return &Answer{Answer: status.Milliliters, Status: ""}, nil
}

func (h HalHandler) GetLevel(ctx context.Context, in *emptypb.Empty) (*AnswerLevel, error) {
	level := h.hal.GetLevel()
	return &AnswerLevel{Answer: int64(level)}, nil
}

func (h HalHandler) Stop(ctx context.Context, in *emptypb.Empty) (*AnswerCommand, error) {
	err := h.hal.DispenserStop()
	if err != nil {
		return &AnswerCommand{Answer: 1}, err
	}
	return &AnswerCommand{Answer: 1}, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
