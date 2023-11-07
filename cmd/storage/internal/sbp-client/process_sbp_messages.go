package sbpclient

import (
	"context"
	"encoding/json"
	"strings"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	paymentEntities "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client/entity/payment"
	rabbit_vo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client/entity/vo"
	"github.com/gofrs/uuid"

	amqp "github.com/rabbitmq/amqp091-go"
)

// ProcessSbpMessage ...
func (s *Service) ProcessSbpMessage(d amqp.Delivery) error {
	// debug
	// fmt.Println(d.Type)
	// fmt.Println(string(d.Body))
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

			if strings.ToLower(msg.Status) == "confirmed" {
				err = s.app.SetPaymentConfirmed(uuid.FromStringOrNil(msg.OrderID))
				if err != nil {
					s.log.PrintErr("SetPaymentConfirmed error", "err", err.Error())
					err = d.Nack(false, false)
					if err != nil {
						return err
					}
					return err
				}
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
	err = s.sendMessage(toPayRequest(payRequest), rabbit_vo.MessageTypePaymentRequest)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

// CancelPayment ...
func (s *Service) CancelPayment(payСancellationRequest app.Payment) (err error) {
	err = s.sendMessage(toPayСancellationRequest(payСancellationRequest), rabbit_vo.MessageTypePaymentCancellationRequest)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

// sendMessage ...
func (s *Service) sendMessage(msg interface{}, messageType rabbit_vo.MessageType) (err error) {
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

	exchangeName := string(rabbit_vo.SbpClientService)
	routingKeyString := string(rabbit_vo.RoutingKeySbpClient)
	return s.sbpClientPub.PublishWithContext(
		context.Background(),
		exchangeName,
		routingKeyString,
		false,
		false,
		message,
	)
}

func toPayRequest(payment app.Payment) paymentEntities.PayRequest {
	return paymentEntities.PayRequest{
		WashID:  payment.ServerID.String(),
		PostID:  payment.PostID.String(),
		OrderID: payment.OrderID.String(),
		Amount:  payment.Amount,
	}
}

func toPayСancellationRequest(payment app.Payment) paymentEntities.PayСancellationRequest {
	return paymentEntities.PayСancellationRequest{
		WashID:  payment.ServerID.String(),
		PostID:  payment.PostID.String(),
		OrderID: payment.OrderID.String(),
	}
}
