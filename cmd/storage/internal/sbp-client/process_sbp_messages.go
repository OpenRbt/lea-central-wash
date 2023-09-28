package sbpclient

import (
	"context"
	"encoding/json"
	"strings"

	paymentEntities "github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/sbp-client/entity/payment"
	rabbit_vo "github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/sbp-client/entity/vo"
	"github.com/gofrs/uuid"

	amqp "github.com/rabbitmq/amqp091-go"
)

// ProcessSbpMessage ...
func (s *Service) ProcessSbpMessage(d amqp.Delivery) error {
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

			if msg.Failed {
				err = s.app.SetPaymentCanceled(uuid.FromStringOrNil(msg.OrderID))
				if err != nil {
					s.log.PrintErr("PaymentResponse error: %s", err.Error())
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

			err = s.app.SetPaymentURL(uuid.FromStringOrNil(msg.OrderID), msg.UrlPay)
			if err != nil {
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
func (s *Service) SendPaymentRequest(payRequest paymentEntities.PayRequest) (err error) {
	return s.sendMessage(payRequest, rabbit_vo.MessageTypePaymentRequest)
}

// CancelPayment ...
func (s *Service) CancelPayment(payСancellationRequest paymentEntities.PayСancellationRequest) (err error) {
	return s.sendMessage(payСancellationRequest, rabbit_vo.MessageTypePaymentCancellationRequest)
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
