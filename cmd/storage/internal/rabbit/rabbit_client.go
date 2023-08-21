package rabbit

import (
	_ "embed"
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	rabbit_vo "github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"
	"github.com/powerman/structlog"
	amqp "github.com/rabbitmq/amqp091-go"
)

type Config struct {
	URL       string
	Port      string
	ServerID  string
	ServerKey string
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
	cfg Config

	conn           *amqp.Connection
	done           chan struct{}
	notifyClose    chan *amqp.Error
	notifyClosePub chan *amqp.Error
	notifyCloseSub chan *amqp.Error

	isConnected int32
	attempts    int8
	serverID    string
	log         *structlog.Logger
	bonusSvcPub *amqp.Channel
	bonusSvcSub *amqp.Channel
	rabbitConf  *amqp.Config
	connString  string
}

func NewClient(cfg Config, app app.App) (svc *Service, err error) {
	//TODO: add rabbit variables extraction from repo

	connString := fmt.Sprintf("amqps://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerKey, cfg.URL, cfg.Port)
	rabbitConf := amqp.Config{
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

// Close re-conn attempts
func (s *Service) Close() error {
	s.Lock()
	defer s.Unlock()
	if atomic.LoadInt32(&s.isConnected) == closed {
		return nil
	}
	atomic.StoreInt32(&s.isConnected, closed)
	close(s.done)

	if s.bonusSvcPub != nil && !s.bonusSvcPub.IsClosed() {
		s.log.ErrIfFail(s.bonusSvcPub.Close)
	}
	if s.bonusSvcSub != nil && !s.bonusSvcSub.IsClosed() {
		s.log.ErrIfFail(s.bonusSvcSub.Close)
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

			s.log.ErrIfFail(s.bonusSvcSub.Close)
			s.log.ErrIfFail(s.bonusSvcPub.Close)
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
	c, err := amqp.DialConfig(s.connString, *s.rabbitConf)

	if err != nil {
		return err
	}
	s.conn = c
	s.notifyClose = c.NotifyClose(make(chan *amqp.Error, 1))

	s.bonusSvcPub, err = c.Channel()
	if err != nil {
		c.Close()
		return err
	}
	s.notifyClosePub = s.bonusSvcPub.NotifyClose(make(chan *amqp.Error, 1))
	if err != nil {
		s.bonusSvcPub.Close()
		c.Close()
		return err
	}

	s.bonusSvcSub, err = c.Channel()
	if err != nil {
		s.bonusSvcPub.Close()
		c.Close()
		return err
	}
	_, err = s.bonusSvcSub.QueueDeclare(
		s.serverID,
		false,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.bonusSvcPub.Close()
		c.Close()
		return err
	}

	err = s.bonusSvcSub.QueueBind(
		s.serverID,
		s.serverID,
		string(rabbit_vo.WashBonusService),
		false,
		nil,
	)
	if err != nil {
		s.bonusSvcPub.Close()
		c.Close()
		return err
	}

	delivery, err := s.bonusSvcSub.Consume(
		s.serverID,
		"",
		false,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.bonusSvcSub.Close()
		s.bonusSvcPub.Close()
		c.Close()
		return err
	}
	s.notifyCloseSub = s.bonusSvcSub.NotifyClose(make(chan *amqp.Error, 1))

	go s.handlerGoroutine(s.bonusSvcSub, delivery, s.ProcessBonusMessage)

	atomic.StoreInt32(&s.isConnected, connected)
	s.log.Info("connected!")
	return nil
}

// IsConnected return connection status
func (s *Service) IsConnected() bool {
	return atomic.LoadInt32(&s.isConnected) == connected
}
