package sbpclient

import (
	_ "embed"
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"

	sbpvo "github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/sbp-client/entity/vo"
	"github.com/powerman/structlog"
	amqp "github.com/rabbitmq/amqp091-go"
)

type RabbitConfig struct {
	URL            string
	Port           string
	ServerID       string
	ServerPassword string
	Secure         bool
}

const (
	disconnected int32 = 0
	connected    int32 = 1
	closed       int32 = -1

	maxAttempts int8 = 5
)

type Service struct {
	sync.Mutex
	app app.App
	cfg RabbitConfig

	conn           *amqp.Connection
	done           chan struct{}
	notifyClose    chan *amqp.Error
	notifyClosePub chan *amqp.Error
	notifyCloseSub chan *amqp.Error

	isConnected  int32
	attempts     int8
	serverID     string
	log          *structlog.Logger
	sbpClientPub *amqp.Channel
	sbpClientSub *amqp.Channel
	rabbitConf   *amqp.Config
	connString   string
}

func NewSbpRabbitClient(cfg RabbitConfig, app app.App) (svc *Service, err error) {
	//TODO: add rabbit variables extraction from repo

	connString := ""
	if cfg.Secure {
		connString = fmt.Sprintf("amqps://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerPassword, cfg.URL, cfg.Port)
	} else {
		connString = fmt.Sprintf("amqp://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerPassword, cfg.URL, cfg.Port)
	}
	rabbitConf := amqp.Config{
		SASL: []amqp.Authentication{
			&amqp.PlainAuth{
				Username: cfg.ServerID,
				Password: cfg.ServerPassword,
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

	svc = &Service{
		app:        app,
		log:        structlog.New(),
		rabbitConf: &rabbitConf,
		connString: connString,
		serverID:   cfg.ServerID,
		cfg:        cfg,
		done:       make(chan struct{}),
	}

	err = svc.connect()
	if err != nil {
		return nil, err
	}

	go svc.recon()
	return
}

// IsConnected return connection status
func (s *Service) IsConnected() bool {
	return atomic.LoadInt32(&s.isConnected) == connected
}

// Close re-conn attempts
func (s *Service) Close() error {
	s.Lock()
	defer s.Unlock()
	if atomic.LoadInt32(&s.isConnected) == closed {
		return nil
	}
	atomic.StoreInt32(&s.isConnected, closed)
	close(s.done)

	if s.sbpClientPub != nil && !s.sbpClientPub.IsClosed() {
		s.log.ErrIfFail(s.sbpClientPub.Close)
	}
	if s.sbpClientSub != nil && !s.sbpClientSub.IsClosed() {
		s.log.ErrIfFail(s.sbpClientSub.Close)
	}

	if s.conn != nil && !s.conn.IsClosed() {
		return s.conn.Close()
	}

	return nil
}

func (s *Service) recon() {
	for {
		for {
			if atomic.LoadInt32(&s.isConnected) == connected {
				break
			}

			if atomic.LoadInt32(&s.isConnected) == closed {
				return
			}

			s.log.ErrIfFail(s.sbpClientSub.Close)
			s.log.ErrIfFail(s.sbpClientPub.Close)
			s.log.ErrIfFail(s.conn.Close)

			if err := s.connect(); err != nil {
				s.log.Err(err)
				if s.attempts < maxAttempts {
					s.attempts++
				}
			} else {
				s.attempts = 0
			}

			time.Sleep(time.Second * time.Duration(s.attempts))
		}

		select {
		case <-s.done:
			return
		case err := <-s.notifyClose:
			s.log.Err(err)
			if atomic.LoadInt32(&s.isConnected) != closed {
				atomic.StoreInt32(&s.isConnected, disconnected)
			}
		case err := <-s.notifyClosePub:
			s.log.Err(err)
			if atomic.LoadInt32(&s.isConnected) != closed {
				atomic.StoreInt32(&s.isConnected, disconnected)
			}
		case err := <-s.notifyCloseSub:
			s.log.Err(err)
			if atomic.LoadInt32(&s.isConnected) != closed {
				atomic.StoreInt32(&s.isConnected, disconnected)
			}
		}
	}
}

func (s *Service) connect() error {
	s.Lock()
	defer s.Unlock()
	s.log.Info("connecting...")

	connection, err := amqp.DialConfig(s.connString, *s.rabbitConf)
	if err != nil {
		return err
	}
	s.conn = connection
	s.notifyClose = connection.NotifyClose(make(chan *amqp.Error, 1))

	// pub
	s.sbpClientPub, err = connection.Channel()
	if err != nil {
		connection.Close()
		return err
	}
	s.notifyClosePub = s.sbpClientPub.NotifyClose(make(chan *amqp.Error, 1))
	if err != nil {
		s.sbpClientPub.Close()
		connection.Close()
		return err
	}

	// sub
	s.sbpClientSub, err = connection.Channel()
	if err != nil {
		s.sbpClientPub.Close()
		connection.Close()
		return err
	}
	_, err = s.sbpClientSub.QueueDeclare(
		s.serverID,
		false,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.sbpClientPub.Close()
		connection.Close()
		return err
	}

	err = s.sbpClientSub.QueueBind(
		s.serverID,
		s.serverID,
		string(sbpvo.LeaWashService),
		false,
		nil,
	)
	if err != nil {
		s.sbpClientPub.Close()
		connection.Close()
		return err
	}

	delivery, err := s.sbpClientSub.Consume(
		s.serverID,
		"",
		false,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.sbpClientSub.Close()
		s.sbpClientPub.Close()
		connection.Close()
		return err
	}
	s.notifyCloseSub = s.sbpClientSub.NotifyClose(make(chan *amqp.Error, 1))

	go s.handlerGoroutine(s.sbpClientSub, delivery, s.ProcessSbpMessage)

	atomic.StoreInt32(&s.isConnected, connected)
	s.log.Info("connected!")
	return nil
}

func (s *Service) handlerGoroutine(consumer *amqp.Channel, msgs <-chan amqp.Delivery, handler func(d amqp.Delivery) (err error)) {
	for msg := range msgs {
		if consumer.IsClosed() {
			break
		}
		err := handler(msg)
		if err != nil {
			s.log.Err(err)
		}

	}
	s.log.Info("rabbit consumer goroutine closed")
}