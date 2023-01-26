package rabbit

import (
	"crypto/tls"
	"crypto/x509"
	_ "embed"
	"fmt"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit/models/vo"
	"github.com/wagslane/go-rabbitmq"
)

type Config struct {
	Url       string
	Port      string
	ServerID  string
	ServerKey string
}

var (
	//go:embed client.pem
	clientCertRaw []byte
	//go:embed client_key.pem
	clientCertKeyRaw []byte
	//go:embed root_ca.pem
	caCertRaw []byte
)

type Service struct {
	app app.App

	conn *rabbitmq.Conn

	bonusSvcPub *rabbitmq.Publisher
	bonusSvcSub *rabbitmq.Consumer
}

func NewClient(cfg Config, app app.App) (svc *Service, err error) {
	rootCAs := x509.NewCertPool()
	rootCAs.AppendCertsFromPEM(caCertRaw)

	cert, err := tls.X509KeyPair(clientCertRaw, clientCertKeyRaw)
	if err != nil {
		return nil, err
	}

	tlsConf := &tls.Config{
		RootCAs:      rootCAs,
		Certificates: []tls.Certificate{cert},
		ServerName:   "localhost", // Optional
	}

	//TODO: add rabbit variables extraction from repo

	connString := fmt.Sprintf("amqps://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerKey, cfg.Url, cfg.Port)
	rabbitConf := rabbitmq.Config{
		SASL:            nil,
		Vhost:           "",
		ChannelMax:      0,
		FrameSize:       0,
		Heartbeat:       0,
		TLSClientConfig: tlsConf,
		Properties:      nil,
		Locale:          "",
		Dial:            nil,
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
		rabbitmq.WithPublisherOptionsExchangeName(vo.WashBonusService),
	)
	if err != nil {
		return
	}

	svc.bonusSvcSub, err = rabbitmq.NewConsumer(conn,
		svc.ProcessBonusMessage,
		cfg.ServerID,
		rabbitmq.WithConsumerOptionsExchangeName(vo.WashBonusService),
		rabbitmq.WithConsumerOptionsConsumerExclusive,
	)

	return
}

func (s *Service) Stop() error {
	return s.conn.Close()
}
