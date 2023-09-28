package app

import (
	"errors"
	"time"
)

// SbpRabbitWorkerConfig ...
type SbpRabbitWorkerConfig struct {
	ServerID                     string
	ServerPassword               string
	SbpRep                       SbpRepInterface
	SbpBroker                    SbpBrokerInterface
	NotificationExpirationPeriod time.Duration
}

// GetSbpConfig ...
func (a *app) GetSbpConfig(envServerSbpID string, envServerSbpPassword string) (cfg SbpRabbitConfig, err error) {
	serverID, err := a.repo.GetConfigString(envServerSbpID)
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	serverPassword, err := a.repo.GetConfigString(envServerSbpPassword)
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	cfg.ServerID = serverID.Value
	cfg.ServerPassword = serverPassword.Value
	return
}

// InitSbpRabbitWorker ...
func (a *app) InitSbpRabbitWorker(config SbpRabbitWorkerConfig) error {
	if config.ServerID == "" {
		return errors.New("InitSbpRabbitWorker: sbpBroker is empty")
	}

	if config.ServerPassword == "" {
		return errors.New("InitSbpRabbitWorker: serviceSbpPassword is empty")
	}

	if config.SbpBroker == nil {
		return errors.New("InitSbpRabbitWorker: sbpBroker = nil")
	}

	if config.SbpRep == nil {
		return errors.New("InitSbpRabbitWorker: SbpRep = nil")
	}

	a.SbpWorker = &SbpWorker{
		serverID:                     config.ServerID,
		sbpRep:                       config.SbpRep,
		sbpBroker:                    config.SbpBroker,
		notificationExpirationPeriod: config.NotificationExpirationPeriod,
	}

	// Cancel Expirated Not OpenwashReceived Payments
	go a.SbpWorker.CancelExpiratedNotOpenwashReceivedPayments()

	return nil
}

// IsSbpRabbitWorkerInit
func (a *app) IsSbpRabbitWorkerInit() bool {
	return a.SbpWorker != nil
}
