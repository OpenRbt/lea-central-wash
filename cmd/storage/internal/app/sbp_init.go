package app

import (
	"fmt"
	"time"
)

// SbpRabbitWorkerConfig ...
type SbpRabbitWorkerConfig struct {
	ServerID                     string
	ServiceSbpKey                string
	SbpRep                       SbpRepInterface
	SbpBroker                    SbpBrokerInterface
	NotificationExpirationPeriod time.Duration
}

// GetSbpConfig ...
func (a *app) GetSbpConfig() (cfg SbpRabbitConfig, err error) {
	serverID, err := a.repo.GetConfigString("server_id")
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	serverKey, err := a.repo.GetConfigString("server_key")
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	serverSbpKey, err := a.repo.GetConfigString("server_sbp_key")
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	cfg.ServerID = serverID.Value
	cfg.ServerPassword = serverKey.Value
	cfg.ServerSbpKey = serverSbpKey.Value
	return
}

// InitSbpRabbitWorker ...
func (a *app) InitSbpRabbitWorker(config SbpRabbitWorkerConfig) error {
	if config.ServerID == "" {
		fmt.Errorf("InitSbpRabbitWorker: sbpBroker is empty")
	}

	if config.ServiceSbpKey == "" {
		fmt.Errorf("InitSbpRabbitWorker: serviceSbpKey is empty")
	}

	if config.SbpBroker == nil {
		fmt.Errorf("InitSbpRabbitWorker: sbpBroker = nil")
	}

	if config.SbpRep == nil {
		fmt.Errorf("InitSbpRabbitWorker: SbpRep = nil")
	}

	a.SbpWorker = SbpWorker{
		serverID:                     config.ServerID,
		serviceSbpKey:                config.ServiceSbpKey,
		sbpRep:                       config.SbpRep,
		sbpBroker:                    config.SbpBroker,
		notificationExpirationPeriod: config.NotificationExpirationPeriod,
	}

	// Cancel Expirated Not OpenwashReceived Payments
	go a.SbpWorker.CancelExpiratedNotOpenwashReceivedPayments()

	return nil
}
