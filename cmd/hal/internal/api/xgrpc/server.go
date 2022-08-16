package xgrpc

import (
	"context"
	"fmt"
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

func (h HalHandler) Command(ctx context.Context, in *OptionsCommand) (*AnswerCommand, error) {
	err := h.hal.Command(int(in.Command))
	fmt.Println("Server-hal: ", err)
	if err != nil {
		fmt.Println("Not NIL")
		return &AnswerCommand{Answer: 1}, err
	}
	fmt.Println("NIL")
	return &AnswerCommand{Answer: 1}, nil
}

func (h HalHandler) Volume(ctx context.Context, in *emptypb.Empty) (*Answer, error) {
	ans, status := h.hal.Volume()
	return &Answer{Answer: ans, Status: status}, nil
}

func (h HalHandler) mustEmbedUnimplementedHardwareAccessLayerServer() {}
