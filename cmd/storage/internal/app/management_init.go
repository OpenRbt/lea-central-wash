package app

import (
	"context"
	"time"
)

// SbpRabbitWorkerConfig ...
type ManagementConfig struct {
	ServerID                     string
	ServerPassword               string
	SbpRep                       SbpRepInterface
	SbpBroker                    SbpBrokerInterface
	NotificationExpirationPeriod time.Duration
}

func (a *app) GetManagementConfig() (cfg ManagementRabbitConfig, err error) {
	serverID, err := a.repo.GetConfigString("management_server_id")
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	serverPassword, err := a.repo.GetConfigString("management_service_key")
	if err != nil {
		err = ErrServiceNotConfigured

		return cfg, err
	}

	cfg.ServerID = serverID.Value
	cfg.ServerPassword = serverPassword.Value
	return
}

func (a *app) InitManagement(mngtWorker ManagementRabbitWorker) {
	a.mngtSvc = management{
		syncChannel:            make(chan struct{}, 1),
		ManagementRabbitWorker: mngtWorker,
	}

	a.syncLeaSettings()

	go a.syncData()
	go a.sendStatus()
	go a.startManagementSync()
}

func (a *app) IsManagementInit() bool {
	return a.mngtSvc.ManagementRabbitWorker != nil
}

func (a *app) IsMngtAvailableForStation(stationID StationID) bool {
	if a.SbpWorker == nil {
		return false
	}
	status := a.mngtSvc.Status()
	return a.isServiceAvailableForStation(stationID, status)
}

func (a *app) syncData() {
	if a.mngtSvc.ManagementRabbitWorker == nil {
		panic("managementSvc == nil")
	}
	for {
		for {
			c, err := a.repo.Collections()
			if err != nil {
				log.Err("collections", "err", err)
				break
			}
			if len(c) == 0 {
				break
			}
			for i := range c {
				err := a.mngtSvc.SendCollectionReport(c[i])
				if err != nil {
					log.Err("send collection", "err", err)
					break
				}
				err = a.repo.CollectionSetSended(c[i].ID)
				if err != nil {
					log.Err("collection set sended", "err", err)
					break
				}
			}
		}

		for {
			c, err := a.repo.MoneyReports()
			if err != nil {
				log.Err("moneyReports", "err", err)
				break
			}
			if len(c) == 0 {
				break
			}
			for i := range c {
				err := a.mngtSvc.SendMoneyReport(c[i])
				if err != nil {
					log.Err("send moneyReport", "err", err)
					break
				}
				err = a.repo.MoneyReportSetSended(c[i].ID)
				if err != nil {
					log.Err("moneyReport set sended", "err", err)
					break
				}
			}
		}
		time.Sleep(time.Second * 5)
	}
}

func (a *app) sendStatus() {
	if a.mngtSvc.ManagementRabbitWorker == nil {
		panic("managementSvc == nil")
	}
	for {
		r := a.StatusReport(true)
		err := a.mngtSvc.SendStatus(r)
		if err != nil {
			log.Err("sendStatus", "err", err)
		}
		time.Sleep(time.Second * 30)
	}
}

func (a *app) startManagementSync() {
	if a.mngtSvc.ManagementRabbitWorker == nil {
		panic("managementRabbitWorker == nil")
	}

	for range a.mngtSvc.syncChannel {
		a.syncLeaSettings()
	}
}

func (a *app) syncLeaSettings() {
	a.syncUnsentPrograms()
	a.syncUnsentAdvertisingCampaigns()
}

func (a *app) syncUnsentPrograms() {
	programs, err := a.NotSendedPrograms(context.TODO())
	if err != nil {
		log.Err("unable to get unsent programs", "err", err)
		return
	}

	for _, program := range programs {
		err := a.mngtSvc.SendProgram(program)
		if err != nil {
			log.Err("unable to send program to management", "err", err)
			continue
		}

		err = a.MarkProgramSended(context.TODO(), program.ID)
		if err != nil {
			log.Err("unable to mark program as sended", "err", err)
			continue
		}
	}
}

func (a *app) syncUnsentAdvertisingCampaigns() {
	campaigns, err := a.repo.NotSendedAdvertisingCampaigns(context.TODO())
	if err != nil {
		log.Err("unable to get unsent advertising campaigns", "err", err)
		return
	}

	for _, campaign := range campaigns {
		err := a.mngtSvc.SendAdvertisingCampaign(campaign)
		if err != nil {
			log.Err("unable to send advertising campaign to management", "err", err)
			continue
		}

		err = a.MarkAdvertisingCampaignSended(context.TODO(), campaign.ID)
		if err != nil {
			log.Err("unable to mark advertising campaign as sended", "err", err)
			continue
		}
	}
}
