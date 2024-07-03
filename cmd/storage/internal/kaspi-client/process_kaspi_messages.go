package sbpclient

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/ping"
	uuid "github.com/satori/go.uuid"
	"github.com/shopspring/decimal"

	amqp "github.com/rabbitmq/amqp091-go"
)

type commandType = string

const (
	commandCheck commandType = "check"
	commandPay   commandType = "pay"
)

type leaStatus = string

const (
	leaStatusOk              leaStatus = "ok"
	leaStatusStationOffline  leaStatus = "offline"
	leaStatusStationNotFound leaStatus = "station_not_found"
	leaStatusWashTimeout     leaStatus = "timeout"
	leaStatusOtherErr        leaStatus = "other"
	leaStatusAccountNotFound leaStatus = "account_not_found"
)

type command struct {
	MessageID    uuid.UUID
	WashServerID uuid.UUID
	StationID    int64
	Type         commandType
	Account      string
	TxnID        int64
	TxnDate      time.Time
	Sum          decimal.Decimal
}

type leaAnswer struct {
	MessageID uuid.UUID
	Status    leaStatus
	Info      string
}

type Message string
type RoutingKey string

const (
	msgCommand           Message = "kaspi_client_service/command"
	commandAnswer        Message = "kaspi_client_service/command_answer"
	kaspiPingMessageType Message = "kaspi_client_service/ping"

	RoutingKeyKaspiPing   RoutingKey = "kaspi_ping"
	RoutingKeyKaspiClient RoutingKey = "kaspi_service_client"
)

func statusFromApp(v app.LeaStatus) leaStatus {
	switch v {
	case app.LeaStatusOk:
		return leaStatusOk
	case app.LeaStatusStationOffline:
		return leaStatusStationOffline
	case app.LeaStatusStationNotFound:
		return leaStatusStationNotFound
	case app.LeaStatusWashTimeout:
		return leaStatusWashTimeout
	case app.LeaStatusOtherErr:
		return leaStatusOtherErr
	case app.LeaStatusAccountNotFound:
		return leaStatusAccountNotFound
	default:
		panic(fmt.Sprintf("unknown status %v", v))
	}
}

func appCommandType(v commandType) app.CommandType {
	switch v {
	case commandCheck:
		return app.CommandCheck
	case commandPay:
		return app.CommandPay
	default:
		panic(fmt.Sprintf("unknown command %v", v))
	}
}

func appCommand(v command) app.Command {
	return app.Command{
		MessageID:    v.MessageID,
		Type:         appCommandType(v.Type),
		WashServerID: v.WashServerID,
		StationID:    v.StationID,
		TxnID:        v.TxnID,
		TxnDate:      v.TxnDate,
		Sum:          v.Sum,
	}
}

// ProcessMessage ...
func (s *Service) ProcessMessage(d amqp.Delivery) error {
	// debug
	fmt.Println(d.Type)
	fmt.Println(string(d.Body))
	switch Message(d.Type) {
	case msgCommand:
		var msg command
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			err = d.Nack(false, false)
			if err != nil {
				return err
			}
			return err
		}

		s.app.KaspiCommand(appCommand(msg))

		err = d.Ack(false)
		if err != nil {
			return err
		}

	default:
		{
			err := d.Nack(false, false)
			if err != nil {
				return err
			}
			return nil
		}
	}

	return nil
}

func (s *Service) SendAnswer(v app.KaspiAnswer) (err error) {
	err = s.sendMessage(leaAnswer{
		MessageID: v.MessageID,
		Info:      v.Info,
		Status:    statusFromApp(v.Status),
	}, commandAnswer, RoutingKeyKaspiClient)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) Ping(serverID string, status []app.StationPingStatus) error {
	p := ping.BonusPing{
		WashID:   serverID,
		Stations: stationPingStatusToRabbit(status),
	}

	return s.sendMessage(p, kaspiPingMessageType, RoutingKeyKaspiPing)
}

func (s *Service) sendMessage(msg interface{}, messageType Message, routingKey RoutingKey) (err error) {
	jsonMsg, ok := msg.([]byte)
	if !ok {
		jsonMsg, err = json.Marshal(msg)
		if err != nil {
			return
		}
	}

	message := amqp.Publishing{}
	message.Body = jsonMsg
	message.Type = string(messageType)
	message.UserId = s.serverID
	message.DeliveryMode = amqp.Persistent

	exchangeName := exchange
	routingKeyString := string(routingKey)

	if messageType == commandAnswer {
		message.Expiration = "4000"
	}

	dConfirmation, err := s.sbpClientPub.PublishWithDeferredConfirmWithContext(
		context.Background(),
		exchangeName,
		routingKeyString,
		false,
		false,
		message,
	)
	if err != nil {
		return err
	}

	Metric.PublicationsTotal.WithLabelValues(string(messageType)).Inc()

	select {
	case <-time.After(time.Second * 5):
		s.sbpClientPub.Nack(dConfirmation.DeliveryTag, false, false)
		fmt.Println("kaspi: timeout")

		s.conn.Close()

		//s.sbpClientPub.Reject(dConfirmation.DeliveryTag, false)
		return app.ErrSendTimeout
	case <-dConfirmation.Done():
		fmt.Println("kaspi: delivered deferred confirm to handler")
		break
	}
	return err
}

func stationPingStatusToRabbit(status []app.StationPingStatus) []ping.StationStatus {
	l := []ping.StationStatus{}
	for _, v := range status {
		l = append(l, ping.StationStatus{
			ID:       int(v.ID),
			IsOnline: v.IsOnline,
		})
	}
	return l
}
