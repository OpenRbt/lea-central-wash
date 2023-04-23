package rabbit

import (
	"encoding/json"
	"errors"

	"github.com/OpenRbt/share_business/wash_rabbit/entity/session"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	rabbit_vo "github.com/OpenRbt/share_business/wash_rabbit/entity/vo"
	"github.com/wagslane/go-rabbitmq"
)

func (s *Service) ProcessBonusMessage(d rabbitmq.Delivery) (action rabbitmq.Action) { // Обработка сообщения на основе типа. В зависимости от типа происходят нужные действия

	switch rabbit_vo.MessageType(d.Type) {
	case rabbit_vo.SessionCreatedMessageType:
		var msg session.PostSessions
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
		err = s.app.AddSessionsToPool(app.StationID(msg.PostID), msg.NewSessions...)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
	case rabbit_vo.SessionUserMessageType:
		var msg session.UserAssign
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}

		err = s.app.AssignSessionUser(msg.SessionID, msg.UserID)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
	case rabbit_vo.SessionBonusChargeMessageType:
		var msg session.BonusCharge
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}

		err = s.app.AssignSessionBonuses(msg.SessionID, int(msg.Amount))
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}

	default:
		action = rabbitmq.NackDiscard
	}

	return
}

func (s *Service) SendMessage(msg interface{}, service rabbit_vo.Service, routingKey rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) (err error) {
	jsonMsg, err := json.Marshal(msg)
	if err != nil {
		return
	}

	serverID, err := s.app.GetConfigString(nil, "server_id")
	if err != nil {
		return err
	}

	switch service {
	case rabbit_vo.WashBonusService:
		return s.bonusSvcPub.Publish(
			jsonMsg,
			[]string{string(routingKey)},
			rabbitmq.WithPublishOptionsExchange(string(rabbit_vo.WashBonusService)),
			rabbitmq.WithPublishOptionsType(string(messageType)),
			rabbitmq.WithPublishOptionsUserID(serverID.Value),
		)
	default:
		return errors.New("unknown service")
	}
}
