package app

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/OpenRbt/share_business/wash_rabbit/entity/session"
	rabbit_vo "github.com/OpenRbt/share_business/wash_rabbit/entity/vo"
)

type BonusRabbitWorker struct {
	repo       Repo
	routingKey string

	publisherFunc func(msg interface{}, service rabbit_vo.Service, target rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) error
}

func (r *BonusRabbitWorker) ProcessMoneyReports() {
	var lastReportID int64

	for {
		lastReportID = 0

		for {
			rabbitMoneyReports, err := r.repo.GetUnsendedMoneyReports(lastReportID)
			if err != nil {
				log.Err("failed to retrieve queued RabbitMoneyReports", "error", err)
			}

			if len(rabbitMoneyReports) == 0 {
				break
			}

			lastReportID = int64(rabbitMoneyReports[int64(len(rabbitMoneyReports)-1)].ID)

			for _, report := range rabbitMoneyReports {
				err := r.publisherFunc(session.MoneyReport{
					StationID:    int(report.MoneyReport.StationID),
					Banknotes:    report.MoneyReport.Banknotes,
					CarsTotal:    report.MoneyReport.CarsTotal,
					Coins:        report.MoneyReport.Coins,
					Electronical: report.MoneyReport.Electronical,
					Service:      report.MoneyReport.Service,
					Bonuses:      report.MoneyReport.Bonuses,
					SessionID:    report.MoneyReport.SessionID,
					UUID:         report.MessageUUID.String(),
				}, rabbit_vo.WashBonusService, rabbit_vo.WashBonusRoutingKey, rabbit_vo.MessageType(report.MessageType))
				if err != nil {
					log.Warn("failed to send RabbitMoneyReports", "error", err)
				} else {
					err = r.repo.MarkRabbitMoneyReportAsSent(int64(report.ID))
					if err != nil {
						log.Err("failed to mark RabbitMoneyReports as sent", "error", err)
					}
				}
			}
		}

		time.Sleep(10 * time.Second)
	}
}

func (r *BonusRabbitWorker) ProcessMessages() {
	var lastMessageID int64

	for {
		lastMessageID = 0

		for {
			messages, err := r.repo.GetUnsendedRabbitMessages(lastMessageID)

			if err != nil {
				log.Err("failed to retrieve queued RabbitMessages", "error", err)
			}

			if len(messages) == 0 {
				break
			}

			lastMessageID = int64(messages[len(messages)-1].ID)

			for _, message := range messages {
				err := r.publisherFunc(message.Payload, rabbit_vo.WashBonusService, rabbit_vo.WashBonusRoutingKey, rabbit_vo.MessageType(message.MessageType))
				if err != nil {
					log.Warn("failed to send persistent RabbitMessage", "error", err)
				} else {
					err = r.repo.MarkRabbitMessageAsSent(int64(message.ID))
					if err != nil {
						log.Err("failed to mark RabbitMessage as sent", "error", err)
					}
				}
			}
		}
		time.Sleep(3 * time.Second)
	}
}

func (a *app) SendMessage(messageType string, payload interface{}) error {
	if a.bonusSystemRabbitWorker == nil {
		return ErrNoRabbitWorker
	}

	return a.bonusSystemRabbitWorker.publisherFunc(payload, rabbit_vo.WashBonusService, rabbit_vo.WashBonusRoutingKey, rabbit_vo.MessageType(messageType))
}

func (a *app) PrepareRabbitMessage(messageType string, payload interface{}) error {
	if payload != nil {
		bytes, err := json.Marshal(payload)
		if err != nil {
			return err
		}

		err = a.repo.AddRabbitMessage(RabbitMessage{
			MessageType: messageType,
			Payload:     bytes,
		})

		return err
	}

	return fmt.Errorf("failed to prepare RabbitMessage to send")
}
