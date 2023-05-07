package app

import (
	"encoding/json"
	"fmt"
	rabbit_vo "github.com/OpenRbt/share_business/wash_rabbit/entity/vo"
	"time"
)

type RabbitWorker struct {
	repo       Repo
	routingKey string

	nonPersistentMessages chan RabbitMessage

	publisherFunc func(msg interface{}, service rabbit_vo.Service, target rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) error
}

func (r *RabbitWorker) ProcessNonPersistentMessages() {
	for {
		msg := <-r.nonPersistentMessages
		err := r.publisherFunc(msg.Payload, rabbit_vo.Service(msg.RoutingKey), rabbit_vo.RoutingKey(msg.Target), rabbit_vo.MessageType(msg.MessageType))
		if err != nil {
			log.Warn("failed to send non-persistent RabbitMessage", "error", err)
		}
	}
}

func (r *RabbitWorker) ProcessPersistentMessages() {
	ticker := time.NewTicker(RabbitWorkerScheduleInterval)
	defer ticker.Stop()
	for {
		<-ticker.C

		messages, err := r.repo.GetUnsendedRabbitMessages(r.routingKey)
		if err != nil {
			log.Err("failed to retrieve queued RabbitMessages", "error", err)
		}

		for _, message := range messages {
			err := r.publisherFunc(message.Payload, rabbit_vo.Service(message.RoutingKey), rabbit_vo.RoutingKey(message.Target), rabbit_vo.MessageType(message.MessageType))
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
}

func (a *app) PrepareRabbitMessage(target string, routingKey string, messageType string, payload interface{}, persistent bool) error {
	if persistent {
		if payload != nil {
			bytes, err := json.Marshal(payload)
			if err != nil {
				return err
			}

			err = a.repo.AddRabbitMessage(RabbitMessage{
				RoutingKey:  routingKey,
				Target:      target,
				MessageType: messageType,
				Payload:     bytes,
			})

			return err
		}
	} else {
		worker, ok := a.rabbitWorkers[routingKey]
		if !ok {
			return fmt.Errorf("rabbit worker not found")
		}
		worker.nonPersistentMessages <- RabbitMessage{
			RoutingKey:  routingKey,
			Target:      target,
			MessageType: messageType,
			Payload:     payload}
		return nil
	}

	return fmt.Errorf("failed to prepare RabbitMessage to send")
}
