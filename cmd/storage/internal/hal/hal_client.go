package hal

import (
	"context"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/api/xgrpc"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/protobuf/types/known/emptypb"
)

type Client struct {
	hal xgrpc.HardwareAccessLayerClient
}

func NewClient() (*Client, error) {
	opt := grpc.WithTransportCredentials(insecure.NewCredentials())

	conn, err := grpc.Dial(":8099", opt)
	if err != nil {
		return nil, err
	}

	client := xgrpc.NewHardwareAccessLayerClient(conn)

	return &Client{
		hal: client,
	}, nil
}

func (c *Client) RunProgram(id int32, cfg app.RelayConfig) (err error) {
	opt := xgrpc.Options{
		ProgramId:         id,
		TimeoutSec:        int32(cfg.TimeoutSec),
		MotorSpeedPercent: int32(cfg.MotorSpeedPercent),
	}

	ctx := context.Background()

	_, err = c.hal.RunProgram(ctx, &opt)

	return err
}

func (c *Client) Command(chi int32) (err error) {
	com := xgrpc.Opti{
		Cmd: chi,
	}
	ctx := context.Background()

	_, err = c.hal.Command(ctx, &com)
	return err
}

func (c *Client) Value() float64 {

	ctx := context.Background()

	ss := emptypb.Empty{}

	com, _ := c.hal.Value(ctx, &ss)

	return com.Otvet
}