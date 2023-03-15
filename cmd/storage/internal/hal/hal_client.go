package hal

import (
	"context"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/api/xgrpc"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/protobuf/types/known/emptypb"
)

type Config struct {
	Endpoint string
}

type Client struct {
	hal xgrpc.HardwareAccessLayerClient
}

func NewClient(config Config) (*Client, error) {
	opt := grpc.WithTransportCredentials(insecure.NewCredentials())

	conn, err := grpc.Dial(config.Endpoint, opt)
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

func (c *Client) MeasureVolumeMilliliters(volume int64, id app.StationID, StartCfg app.RelayConfig, StopCfg app.RelayConfig) (err error) {
	StartRel := []*xgrpc.Relay{}
	for _, entry := range StartCfg.Timings {
		z := &xgrpc.Relay{
			ID:      int32(entry.ID),
			TimeOn:  int32(entry.TimeOn),
			TimeOff: int32(entry.TimeOff),
		}
		StartRel = append(StartRel, z)
	}

	StopRel := []*xgrpc.Relay{}
	for _, entry := range StopCfg.Timings {
		z := &xgrpc.Relay{
			ID:      int32(entry.ID),
			TimeOn:  int32(entry.TimeOn),
			TimeOff: int32(entry.TimeOff),
		}
		StopRel = append(StopRel, z)
	}

	com := xgrpc.OptionsCommand{
		Volume:                 int32(volume),
		StationId:              int32(id),
		StartTimeoutSec:        int32(StartCfg.TimeoutSec),
		StartMotorSpeedPercent: int32(StartCfg.MotorSpeedPercent),
		StartRelays:            StartRel,
		StopTimeoutSec:         int32(StopCfg.TimeoutSec),
		StopMotorSpeedPercent:  int32(StopCfg.MotorSpeedPercent),
		StopRelays:             StopRel,
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

func (c *Client) GetLevel() (int64, error) {
	ctx := context.Background()

	ss := emptypb.Empty{}

	com, err := c.hal.GetLevel(ctx, &ss)

	return com.Answer, err
}

func (c *Client) DispenserStop(id app.StationID, cfg app.RelayConfig) (err error) {
	rel := []*xgrpc.Relay{}

	for _, entry := range cfg.Timings {
		z := &xgrpc.Relay{
			ID:      int32(entry.ID),
			TimeOn:  int32(entry.TimeOn),
			TimeOff: int32(entry.TimeOff),
		}
		rel = append(rel, z)
	}

	opt := xgrpc.RequestStopDispenser{
		StationId:         int32(id),
		TimeoutSec:        int32(cfg.TimeoutSec),
		MotorSpeedPercent: int32(cfg.MotorSpeedPercent),
		Relays:            rel,
	}

	ctx := context.Background()

	_, err = c.hal.Stop(ctx, &opt)
	return err
}
