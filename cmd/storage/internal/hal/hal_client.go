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

	rel := []*xgrpc.Relay{}

	for _, entry := range cfg.Timings {
		z := &xgrpc.Relay{
			ID:      int32(entry.ID),
			TimeOn:  int32(entry.TimeOn),
			TimeOff: int32(entry.TimeOff),
		}
		rel = append(rel, z)
	}

	opt := xgrpc.Options{
		StationId:         id,
		TimeoutSec:        int32(cfg.TimeoutSec),
		MotorSpeedPercent: int32(cfg.MotorSpeedPercent),
		Relays:            rel,
	}

	ctx := context.Background()

	_, err = c.hal.RunProgram(ctx, &opt)

	return err
}

func (c *Client) MeasureVolumeMilliliters(volume int64) (err error) {
	com := xgrpc.OptionsCommand{
		Command: int32(volume),
	}

	ctx := context.Background()

	_, err = c.hal.MeasureVolumeMilliliters(ctx, &com)
	return err
}

func (c *Client) Volume() (int64, string, error) {

	ctx := context.Background()

	ss := emptypb.Empty{}

	com, err := c.hal.Volume(ctx, &ss)

	return com.Answer, com.Status, err
}
