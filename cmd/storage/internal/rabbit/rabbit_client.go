package rabbit

import (
	_ "embed"
	"fmt"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"
	amqp "github.com/rabbitmq/amqp091-go"
	"github.com/wagslane/go-rabbitmq"
)

type Config struct {
	URL       string
	Port      string
	ServerID  string
	ServerKey string
}

type Service struct {
	app app.App

	conn *rabbitmq.Conn

	bonusSvcPub *rabbitmq.Publisher
	bonusSvcSub *rabbitmq.Consumer
}

func NewClient(cfg Config, app app.App) (svc *Service, err error) {
	//TODO: add rabbit variables extraction from repo

	connString := fmt.Sprintf("amqps://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerKey, cfg.URL, cfg.Port)
	rabbitConf := rabbitmq.Config{
		SASL: []amqp.Authentication{
			&amqp.PlainAuth{
				Username: cfg.ServerID,
				Password: cfg.ServerKey,
			},
		},
		Vhost:      "/",
		ChannelMax: 0,
		FrameSize:  0,
		Heartbeat:  0,
		Properties: nil,
		Locale:     "",
		Dial:       nil,
	}

	conn, err := rabbitmq.NewConn(
		connString,
		rabbitmq.WithConnectionOptionsLogging,
		rabbitmq.WithConnectionOptionsConfig(rabbitConf),
	)
	if err != nil {
		return nil, err
	}

	svc = &Service{
		conn: conn,
		app:  app,
	}

	svc.bonusSvcPub, err = rabbitmq.NewPublisher(conn,
		rabbitmq.WithPublisherOptionsLogging,
		rabbitmq.WithPublisherOptionsExchangeName(string(vo.WashBonusService)),
	)
	if err != nil {
		return
	}

	svc.bonusSvcSub, err = rabbitmq.NewConsumer(conn,
		svc.ProcessBonusMessage,
		cfg.ServerID,
		rabbitmq.WithConsumerOptionsExchangeName(string(vo.WashBonusService)),
		rabbitmq.WithConsumerOptionsRoutingKey(cfg.ServerID),
	)

	return
}

func (s *Service) Stop() error {
	return s.conn.Close()
}
