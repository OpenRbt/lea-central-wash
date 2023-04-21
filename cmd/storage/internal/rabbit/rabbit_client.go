package rabbit

import (
	"crypto/tls"
	"crypto/x509"
	_ "embed"
	"fmt"
	"io/ioutil"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/share_business/wash_rabbit/entity/vo"
	"github.com/wagslane/go-rabbitmq"
)

type Config struct {
	Url       string
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

func NewClient(cfg Config, app app.App, rabbitCertPath string) (svc *Service, err error) {

	clientCertRaw, err := ioutil.ReadFile(rabbitCertPath + "client.pem")
	if err != nil {
		return nil, err
	}

	clientCertKeyRaw, err := ioutil.ReadFile(rabbitCertPath + "client_key.pem")
	if err != nil {
		return nil, err
	}

	caCertRaw, err := ioutil.ReadFile(rabbitCertPath + "root_ca.pem")
	if err != nil {
		return nil, err
	}

	rootCAs := x509.NewCertPool()
	rootCAs.AppendCertsFromPEM(caCertRaw)

	cert, err := tls.X509KeyPair(clientCertRaw, clientCertKeyRaw)
	if err != nil {
		return nil, err
	}

	tlsConf := &tls.Config{
		RootCAs:            rootCAs,
		Certificates:       []tls.Certificate{cert},
		ServerName:         "localhost", // Optional
		InsecureSkipVerify: true,
	}

	//TODO: add rabbit variables extraction from repo

	connString := fmt.Sprintf("amqps://%s:%s@%s:%s/", cfg.ServerID, cfg.ServerKey, cfg.Url, cfg.Port)
	fmt.Println("Config string ", connString)
	rabbitConf := rabbitmq.Config{
		SASL:            nil,
		Vhost:           "/",
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
		rabbitmq.WithPublisherOptionsExchangeName(string(vo.WashBonusService)),
	)
	if err != nil {
		return
	}

	svc.bonusSvcSub, err = rabbitmq.NewConsumer(conn,
		svc.ProcessBonusMessage,
		cfg.ServerID,
		rabbitmq.WithConsumerOptionsExchangeName(string(vo.WashBonusService)),
		rabbitmq.WithConsumerOptionsConsumerExclusive,
		rabbitmq.WithConsumerOptionsRoutingKey(cfg.ServerID),
	)

	return
}

func (s *Service) Stop() error {
	return s.conn.Close()
}
