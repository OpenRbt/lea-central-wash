package app

import (
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

func (a *app) InitManagement(svc ManagementService) {
	a.managementSvc = svc
	go a.syncData()
	go a.sendStatus()
}

func (a *app) IsManagementInit() bool {
	return a.managementSvc != nil
}

func (a *app) IsMngtAvailableForStation(stationID StationID) bool {
	if a.SbpWorker == nil {
		return false
	}
	status := a.managementSvc.Status()
	return a.isServiceAvailableForStation(stationID, status)
}

func (a *app) syncData() {
	if a.managementSvc == nil {
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
				err := a.managementSvc.SendCollectionReport(c[i])
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
				err := a.managementSvc.SendMoneyReport(c[i])
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
	if a.managementSvc == nil {
		panic("managementSvc == nil")
	}
	for {
		r := a.StatusReport(true)
		err := a.managementSvc.SendStatus(r)
		if err != nil {
			log.Err("sendStatus", "err", err)
		}
		time.Sleep(time.Second * 30)
	}
}
