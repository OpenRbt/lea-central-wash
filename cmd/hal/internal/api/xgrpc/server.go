package xgrpc

import (
	"context"
	"fmt"

	"github.com/OpenRbt/lea-central-wash/cmd/hal/internal/app"

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
	startRelay := []app.Relay{}
	for _, entry := range in.StartRelays {
		z := app.Relay{
			ID:      int(entry.ID),
			TimeOn:  int(entry.TimeOn),
			TimeOff: int(entry.TimeOff),
		}
		startRelay = append(startRelay, z)
	}

	stopRelay := []app.Relay{}
	for _, entry := range in.StopRelays {
		z := app.Relay{
			ID:      int(entry.ID),
			TimeOn:  int(entry.TimeOn),
			TimeOff: int(entry.TimeOff),
		}
		stopRelay = append(stopRelay, z)
	}

	err := h.hal.MeasureVolumeMilliliters(int(in.Volume), in.StationId, app.RelayConfig{
		MotorSpeedPercent: in.StartMotorSpeedPercent,
		TimeoutSec:        in.StartTimeoutSec,
		Timings:           startRelay,
	}, app.RelayConfig{
		MotorSpeedPercent: in.StopMotorSpeedPercent,
		TimeoutSec:        in.StopTimeoutSec,
		Timings:           stopRelay,
	})
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

func (h HalHandler) Stop(ctx context.Context, in *RequestStopDispenser) (*AnswerCommand, error) {
	relay := []app.Relay{}
	for _, entry := range in.Relays {
		z := app.Relay{
			ID:      int(entry.ID),
			TimeOn:  int(entry.TimeOn),
			TimeOff: int(entry.TimeOff),
		}
		relay = append(relay, z)
	}

	err := h.hal.DispenserStop(app.RelayConfig{
		MotorSpeedPercent: in.MotorSpeedPercent,
		TimeoutSec:        in.TimeoutSec,
		Timings:           relay,
	})
	if err != nil {
		return &AnswerCommand{Answer: 1}, err
	}
	return &AnswerCommand{Answer: 1}, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
