package app

import (
	"errors"
	"time"
)

// SbpRabbitWorkerConfig ...
type SbpRabbitWorkerConfig struct {
	ServerID                      string
	ServerPassword                string
	SbpRep                        SbpRepInterface
	SbpBroker                     SbpBrokerInterface
	NotificationExpirationPeriod  time.Duration
	PaymentConfirmationPingPeriod time.Duration
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
		return errors.New("InitSbpRabbitWorker: sbp broker is empty")
	}

	if config.ServerPassword == "" {
		return errors.New("InitSbpRabbitWorker: service sbp password is empty")
	}

	if config.SbpBroker == nil {
		return errors.New("InitSbpRabbitWorker: sbp broker = nil")
	}

	if config.SbpRep == nil {
		return errors.New("InitSbpRabbitWorker: sbp rep = nil")
	}

	a.SbpWorker = &SbpWorker{
		serverID:                      config.ServerID,
		sbpRep:                        config.SbpRep,
		sbpBroker:                     config.SbpBroker,
		notificationExpirationPeriod:  config.NotificationExpirationPeriod,
		paymentConfirmationPingPeriod: config.PaymentConfirmationPingPeriod,
		sendConfirmRequestChan:        make(chan struct{}, 50),
		sendConfirmRequestTiker:       time.NewTicker(time.Second * 5),
	}

	// Cancel Expirated Not OpenwashReceived Payments
	a.SbpWorker.CancelExpiratedNotOpenwashReceivedPayments()
	// repeat
	go Repeat(a.SbpWorker.CancelExpiratedNotOpenwashReceivedPayments, time.Minute)
	go a.confirmPayment()

	return nil
}

// IsSbpRabbitWorkerInit
func (a *app) IsSbpRabbitWorkerInit() bool {
	return a.SbpWorker != nil
}

func (a *app) IsSbpAvailable() bool {
	if a.SbpWorker == nil {
		return false
	}
	status := a.SbpWorker.sbpBroker.Status()
	return status.IsAvailable()
}
