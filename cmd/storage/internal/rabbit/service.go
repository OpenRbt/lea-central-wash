package rabbit

import (
	"encoding/json"
	"errors"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit/models"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit/models/vo"
	"github.com/wagslane/go-rabbitmq"
)

func (s *Service) ProcessBonusMessage(d rabbitmq.Delivery) (action rabbitmq.Action) { // Обработка сообщения на основе типа. В зависимости от типа происходят нужные действия
	messageType := vo.MessageTypeFromString(d.Type)
	switch messageType {
	case vo.BonusSessionCreated:
		var msg models.SessionCreation
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
	case vo.BonusSessionUserAssign:
		var msg models.SessionUserAssign
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
	case vo.BonusSessionBonusCharge:
		var msg models.SessionBonusCharge
		err := json.Unmarshal(d.Body, &msg)
		if err != nil {
			action = rabbitmq.NackDiscard
			return
		}
	default:
		action = rabbitmq.NackDiscard
	}

	return
}

func (s *Service) SendMessage(msg interface{}, service string, target string, messageType int) (err error) {
	jsonMsg, err := json.Marshal(msg)
	if err != nil {
		return
	}

	switch service {
	case vo.WashBonusService:
		return s.bonusSvcPub.Publish(
			jsonMsg,
			[]string{target},
			rabbitmq.WithPublishOptionsType(vo.MessageType(messageType).String()),
		)
	default:
		return errors.New("unknown service")
	}
}
