package sbpclient

import (
	_ "embed"
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"

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
	exchange         = "management_service"
)

type StationID app.StationID
type Service struct {
	sync.Mutex
	app app.App
	cfg RabbitConfig

	conn           *amqp.Connection
	done           chan struct{}
	notifyClose    chan *amqp.Error
	notifyClosePub chan *amqp.Error
	notifyCloseSub chan *amqp.Error

	isConnected   int32
	attempts      int8
	serverID      string
	log           *structlog.Logger
	mngtClientPub *amqp.Channel
	mngtClientSub *amqp.Channel
	rabbitConf    *amqp.Config
	connString    string

	statusMu       sync.Mutex
	isPaid         bool
	isEnabled      bool
	lastErr        string
	dateLastErr    *time.Time
	reconnectCount int32
}

var _ = app.ManagementRabbitWorker(&Service{})

func NewMngtRabbitClient(cfg RabbitConfig, a app.App) (svc *Service, err error) {
	v, err := a.GetConfigString(nil, "management_server_id")
	if err != nil || v.Value == "" {
		err = app.ErrServiceNotConfigured
		return nil, err
	}
	cfg.ServerID = string(v.Value)
	v, err = a.GetConfigString(nil, "management_service_key")
	if err != nil || v.Value == "" {
		err = app.ErrServiceNotConfigured
		return nil, err
	}
	cfg.ServerPassword = v.Value

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
		Heartbeat:  10 * time.Second,
		Properties: nil,
		Locale:     "",
		Dial:       nil,
	}

	svc = &Service{
		app:         a,
		log:         structlog.New(),
		rabbitConf:  &rabbitConf,
		connString:  connString,
		serverID:    cfg.ServerID,
		isPaid:      false,
		isEnabled:   false,
		cfg:         cfg,
		done:        make(chan struct{}),
		lastErr:     "",
		dateLastErr: nil,
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

// Status return service status
func (s *Service) Status() app.ServiceStatus {
	s.statusMu.Lock()
	if s.dateLastErr != nil {
		if time.Now().UTC().Sub(*s.dateLastErr) > 24*time.Hour {
			s.dateLastErr = nil
			s.lastErr = ""
		}
	}
	defer s.statusMu.Unlock()
	return app.ServiceStatus{
		Available:      true,
		IsPaid:         s.isPaid,
		IsEnabled:      s.isEnabled,
		LastErr:        s.lastErr,
		DateLastErr:    s.dateLastErr,
		IsConnected:    atomic.LoadInt32(&s.isConnected) == connected,
		ReconnectCount: atomic.LoadInt32(&s.reconnectCount),
	}
}

func (s *Service) setLastErr(err string) {
	s.statusMu.Lock()
	t := time.Now().UTC()
	s.dateLastErr = &t
	s.lastErr = err
	s.statusMu.Unlock()
	Metric.ErrsTotal.Inc()
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

	if s.mngtClientPub != nil && !s.mngtClientPub.IsClosed() {
		s.log.ErrIfFail(s.mngtClientPub.Close)
	}
	if s.mngtClientSub != nil && !s.mngtClientSub.IsClosed() {
		s.log.ErrIfFail(s.mngtClientSub.Close)
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

			s.log.ErrIfFail(s.mngtClientSub.Close)
			s.log.ErrIfFail(s.mngtClientPub.Close)
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
				s.addReconnect()
			}
		case err := <-s.notifyClosePub:
			s.log.Err(err)
			if atomic.LoadInt32(&s.isConnected) != closed {
				atomic.StoreInt32(&s.isConnected, disconnected)
				s.addReconnect()
			}
		case err := <-s.notifyCloseSub:
			s.log.Err(err)
			if atomic.LoadInt32(&s.isConnected) != closed {
				atomic.StoreInt32(&s.isConnected, disconnected)
				s.addReconnect()
			}
		}
	}
}

func (s *Service) addReconnect() {
	v := atomic.LoadInt32(&s.reconnectCount)
	atomic.StoreInt32(&s.reconnectCount, v+1)
	Metric.ReconnectTotal.Inc()
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
	s.mngtClientPub, err = connection.Channel()
	if err != nil {
		connection.Close()
		return err
	}
	s.notifyClosePub = s.mngtClientPub.NotifyClose(make(chan *amqp.Error, 1))

	// sub
	s.mngtClientSub, err = connection.Channel()
	if err != nil {
		s.mngtClientPub.Close()
		connection.Close()
		return err
	}
	_, err = s.mngtClientSub.QueueDeclare(
		s.serverID,
		true,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.mngtClientPub.Close()
		connection.Close()
		return err
	}

	err = s.mngtClientPub.Confirm(false)
	if err != nil {
		s.mngtClientPub.Close()
		connection.Close()
		return err
	}
	err = s.mngtClientSub.QueueBind(
		s.serverID,
		s.serverID,
		exchange,
		false,
		nil,
	)
	if err != nil {
		s.mngtClientPub.Close()
		connection.Close()
		return err
	}

	delivery, err := s.mngtClientSub.Consume(
		s.serverID,
		"",
		false,
		false,
		false,
		false,
		amqp.Table{},
	)
	if err != nil {
		s.mngtClientSub.Close()
		s.mngtClientPub.Close()
		connection.Close()
		return err
	}
	s.notifyCloseSub = s.mngtClientSub.NotifyClose(make(chan *amqp.Error, 1))

	go s.handlerGoroutine(s.mngtClientSub, delivery, s.ProcessMngtMessage)

	atomic.StoreInt32(&s.isConnected, connected)
	s.log.Info("connected!")
	return nil
}

func (s *Service) handlerGoroutine(consumer *amqp.Channel, msgs <-chan amqp.Delivery, handler func(d amqp.Delivery) (err error)) {
	for msg := range msgs {
		if consumer.IsClosed() {
			break
		}

		t := time.Now()

		err := handler(msg)
		if err != nil {
			s.log.PrintErr("handlerGoroutine", "err", err)
			Metric.DeliveriesErrsTotal.WithLabelValues(msg.Type).Inc()
		}

		Metric.DeliveriesDuration.WithLabelValues(msg.Type).Observe(time.Since(t).Seconds())
		Metric.DeliveriesTotal.WithLabelValues(msg.Type).Inc()
	}
	s.log.Info("rabbit consumer goroutine closed")
}
