package sbpclient

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/ping"
	paymentEntities "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client/entity/payment"
	rabbit_vo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client/entity/vo"
	uuid "github.com/satori/go.uuid"

	amqp "github.com/rabbitmq/amqp091-go"
)

// ProcessSbpMessage ...
func (s *Service) ProcessSbpMessage(d amqp.Delivery) error {
	// debug
	// fmt.Println(d.Type)
	fmt.Println(string(d.Body))
	switch rabbit_vo.MessageType(d.Type) {

	// payment response
	case rabbit_vo.MessageTypePaymentResponse:
		{
			var msg paymentEntities.PayResponse
			err := json.Unmarshal(d.Body, &msg)
			if err != nil {
				err = d.Nack(false, false)
				if err != nil {
					return err
				}
				return err
			}

			if msg.Error != "" {
				s.log.PrintErr("Server response error", "err", msg.Error)
				s.setLastErr(msg.Error)
			}
			if msg.Failed {
				err = s.app.SetPaymentCanceled(uuid.FromStringOrNil(msg.OrderID))
				if err != nil {
					s.log.PrintErr("PaymentResponse error", "err", err.Error())
					err = d.Nack(false, false)
					if err != nil {
						return err
					}
					return err
				}

				err = d.Ack(false)
				if err != nil {
					return err
				}
				return nil
			}

			err = s.app.SetPaymentURL(uuid.FromStringOrNil(msg.OrderID), msg.UrlPay)
			if err != nil {
				s.log.PrintErr("SetPaymentURL error", "err", err.Error())
				err = d.Nack(false, false)
				if err != nil {
					return err
				}
				return err
			}

			err = d.Ack(false)
			if err != nil {
				return err
			}
		}

	// payment notification
	case rabbit_vo.MessageTypePaymentNotification:
		{
			var msg paymentEntities.PayNotifcation
			err := json.Unmarshal(d.Body, &msg)
			if err != nil {
				err = d.Nack(false, false)
				if err != nil {
					return err
				}
				return err
			}

			status, err := toPaymentStatus(strings.ToLower(msg.Status))
			if err != nil {
				return err
			}

			err = s.app.ReceiveNotification(uuid.FromStringOrNil(msg.OrderID), status)
			if err != nil {
				s.log.PrintErr("SetPaymentConfirmed error", "err", err.Error())
				err = d.Nack(false, false)
				if err != nil {
					return err
				}
				return err
			}

			err = d.Ack(false)
			if err != nil {
				return err
			}
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

// SendPaymentRequest ...
func (s *Service) SendPaymentRequest(payRequest app.Payment) (err error) {
	err = s.sendMessage(toPayRequest(payRequest), rabbit_vo.MessageTypePaymentRequest, rabbit_vo.RoutingKeySbpClient)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

// CancelPayment ...
func (s *Service) CancelPayment(payСancellationRequest app.Payment, errMsg string) (err error) {
	err = s.sendMessage(toPayСancellationRequest(payСancellationRequest, errMsg), rabbit_vo.MessageTypePaymentCancellationRequest, rabbit_vo.RoutingKeySbpClient)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) ConfirmPayment(payConfirmRequest app.Payment) error {
	err := s.sendMessage(toPayConfirmationRequest(payConfirmRequest), rabbit_vo.PaymentConfirmationRequestMessageType, rabbit_vo.RoutingKeySbpClient)
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

	return s.sendMessage(p, rabbit_vo.SBPPingMessageType, rabbit_vo.RoutingKeySbpPing)
}

// sendMessage ...
func (s *Service) sendMessage(msg interface{}, messageType rabbit_vo.MessageType, routingKey rabbit_vo.RoutingKey) (err error) {
	var jsonMsg []byte
	if msg != nil {
		var ok bool
		jsonMsg, ok = msg.([]byte)
		if !ok {
			jsonMsg, err = json.Marshal(msg)
			if err != nil {
				return
			}
		}
	}

	message := amqp.Publishing{}
	message.Body = jsonMsg
	message.Type = string(messageType)
	message.UserId = s.serverID
	message.DeliveryMode = amqp.Persistent
	exchangeName := string(rabbit_vo.SbpClientService)
	routingKeyString := string(routingKey)

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
	select {
	case <-time.After(time.Second * 10):
		return app.ErrSendTimeout
	case <-dConfirmation.Done():
		fmt.Println("sbp: delivered deferred confirm to handler")
		break
	}
	return err
}

func toPayRequest(payment app.Payment) paymentEntities.PayRequest {
	return paymentEntities.PayRequest{
		WashID:  payment.ServerID.String(),
		PostID:  payment.PostID.String(),
		OrderID: payment.OrderID.String(),
		Amount:  payment.Amount,
		Version: 1,
	}
}

func toPayСancellationRequest(payment app.Payment, err string) paymentEntities.PayСancellationRequest {
	return paymentEntities.PayСancellationRequest{
		WashID:  payment.ServerID.String(),
		PostID:  payment.PostID.String(),
		OrderID: payment.OrderID.String(),
		Error:   err,
	}
}

func toPayConfirmationRequest(payment app.Payment) paymentEntities.PaymentConfirmationRequest {
	return paymentEntities.PaymentConfirmationRequest{
		OrderID: payment.OrderID.String(),
	}
}

func toPaymentStatus(status string) (app.PaymentStatus, error) {
	s := app.PaymentStatus(status)
	switch s {
	case app.PaymentStatusAuthorized, app.PaymentStatusCanceled, app.PaymentStatusConfirmed, app.PaymentStatusRefunded, app.PaymentStatusRejected, app.PaymentStatusReversed:
		return s, nil
	default:
		return app.PaymentStatus(""), app.ErrWrongPaymentStatus
	}
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
