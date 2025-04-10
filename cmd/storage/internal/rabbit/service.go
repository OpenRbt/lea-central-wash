package rabbit

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/session"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	rabbit_vo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"
	amqp "github.com/rabbitmq/amqp091-go"
)

func (s *Service) ProcessBonusMessage(d amqp.Delivery) error { // Обработка сообщения на основе типа. В зависимости от типа происходят нужные действия
	switch rabbit_vo.MessageType(d.Type) {
	case rabbit_vo.ServiceStatusMessageType:
		var msg session.ServiceStatus
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			d.Nack(false, false)
			return err
		}

		s.statusMu.Lock()
		s.isPaid = msg.IsPaid
		s.isEnabled = msg.IsEnabled
		s.statusMu.Unlock()

		s.log.Info("new bonus status", "is paid", msg.IsPaid, "is enabled", msg.IsEnabled)

		d.Ack(false)

	case rabbit_vo.SessionCreatedMessageType:
		var msg session.PostSessions
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			d.Nack(false, false)
			return err
		}
		err = s.app.AddSessionsToPool(app.StationID(msg.PostID), msg.NewSessions...)
		if err != nil {
			d.Nack(false, false)
			return err
		}
		d.Ack(false)
	case rabbit_vo.SessionUserMessageType:
		var msg session.UserAssign
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			d.Nack(false, false)
			return err
		}

		err = s.app.AssignSessionUser(msg.SessionID, msg.UserID, app.StationID(msg.Post))
		if err != nil {
			d.Nack(false, false)
			return err
		}
		d.Ack(false)
	case rabbit_vo.SessionBonusChargeMessageType:
		var msg session.BonusCharge
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			d.Nack(false, false)
			return err
		}

		err = s.app.AssignSessionBonuses(msg.SessionID, int(msg.Amount), app.StationID(msg.Post))
		if err != nil {
			d.Nack(false, false)
			return err
		}
		d.Ack(false)
	case rabbit_vo.WashServerDeletionMessageType:
		err := s.app.DeleteConfigString(nil, "server_id")
		if err != nil {
			d.Nack(false, false)

			return err
		}

		err = s.app.DeleteConfigString(nil, "server_key")
		if err != nil {
			d.Nack(false, false)

			return err
		}

		d.Ack(false)

	default:
		d.Nack(false, false)
		return nil
	}

	return nil
}

func (s *Service) SendMessage(msg interface{}, service rabbit_vo.Service, routingKey rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) (err error) {
	jsonMsg, ok := msg.([]byte)
	if !ok {
		jsonMsg, err = json.Marshal(msg)
		if err != nil {
			return
		}
	}

	serverID, err := s.app.GetConfigString(nil, "server_id")
	if err != nil {
		return err
	}

	message := amqp.Publishing{}
	message.Body = jsonMsg
	message.Type = string(messageType)
	message.UserId = serverID.Value

	switch service {
	case rabbit_vo.WashBonusService:
		err = s.bonusSvcPub.PublishWithContext(
			context.Background(),
			string(rabbit_vo.WashBonusService),
			string(routingKey),
			false,
			false,
			message,
		)
		if err != nil {
			s.setLastErr(err.Error())
		} else {
			Metric.PublicationsTotal.WithLabelValues(string(messageType)).Inc()
		}
		return err
	default:
		return errors.New("unknown service")
	}
}
