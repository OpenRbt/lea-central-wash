package app

import (
	"context"
	"errors"
	"fmt"
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

func (a *app) IsMngtAvailable() bool {
	if !a.IsManagementInit() {
		return false
	}
	status := a.mngtSvc.Status()
	return status.IsAvailable()
}

func (a *app) handleCollectionReportSync(report CollectionReport) error {
	if err := a.mngtSvc.SendCollectionReport(report); err != nil {
		if errors.Is(err, ErrNotConfirmed) {
			return nil
		}
		return fmt.Errorf("send collection report: %w", err)
	}

	if err := a.repo.CollectionSetSended(report.ID); err != nil {
		return fmt.Errorf("set collection report as sent: %w", err)
	}

	return nil
}

func (a *app) handleMoneyReportSync(report MngtMoneyReport) error {
	if err := a.mngtSvc.SendMoneyReport(report); err != nil {
		if errors.Is(err, ErrNotConfirmed) {
			return nil
		}
		return fmt.Errorf("send money report: %w", err)
	}

	if err := a.repo.MoneyReportSetSended(report.ID); err != nil {
		return fmt.Errorf("set money report as sent: %w", err)
	}

	return nil
}

func (a *app) syncData() {
	if a.mngtSvc.ManagementRabbitWorker == nil {
		panic("managementSvc == nil")
	}
	for {
		if !a.IsMngtAvailable() {
			time.Sleep(time.Second * 15)
			continue
		}

		for {
			if !a.IsMngtAvailable() {
				break
			}

			c, err := a.repo.Collections()
			if err != nil {
				log.Err("collections", "err", err)
				break
			}
			if len(c) == 0 {
				break
			}
			for _, report := range c {
				if err := a.handleCollectionReportSync(report); err != nil {
					log.Err("handle collectionReport", "err", err)
					break
				}
			}
		}

		for {
			if !a.IsMngtAvailable() {
				break
			}

			c, err := a.repo.MoneyReports()
			if err != nil {
				log.Err("moneyReports", "err", err)
				break
			}
			if len(c) == 0 {
				break
			}
			for _, report := range c {
				if err := a.handleMoneyReportSync(report); err != nil {
					log.Err("handle moneyReport", "err", err)
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

	if a.IsMngtAvailable() {
		r := a.StatusReport(true, true)
		err := a.mngtSvc.SendStatus(r, true)
		if err != nil {
			log.Err("sendStatus", "err", err)
		}
		time.Sleep(time.Second * 15)
	}

	for {
		if a.IsMngtAvailable() {
			r := a.StatusReport(true, true)
			err := a.mngtSvc.SendStatus(r, false)
			if err != nil {
				log.Err("sendStatus", "err", err)
			}
		}
		time.Sleep(time.Second * 15)
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
	if a.IsMngtAvailable() {
		a.syncUnsentStations()
		a.syncUnsentPrograms()
		a.syncUnsentAdvertisingCampaigns()
		a.syncUnsentOpenwashingLogs()
		a.syncUnsentConfigs()
		a.syncUnsentUsers()
		a.syncUnsentTasks()
	}
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

func (a *app) syncUnsentOpenwashingLogs() {
	logs, err := a.repo.NotSendedOpenwashingLogs(context.TODO())
	if err != nil {
		log.Err("unable to get unsent logs", "err", err)
		return
	}

	for _, l := range logs {
		err := a.mngtSvc.SendOpenwashingLog(l)
		if err != nil {
			log.Err("unable to send program to management", "err", err)
			continue
		}

		err = a.repo.MarkOpenwashingLogSended(context.TODO(), l.ID)
		if err != nil {
			log.Err("unable to mark log as sended", "err", err)
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

func (a *app) syncUnsentUsers() {
	users, err := a.repo.NotSendedUsers(context.TODO())
	if err != nil {
		log.Err("unable to get unsent users", "err", err)
		return
	}

	for _, user := range users {
		err := a.mngtSvc.SendUser(user)
		if err != nil {
			log.Err("unable to send user to management", "err", err)
			continue
		}

		err = a.repo.MarkUserSended(context.TODO(), user.Login)
		if err != nil {
			log.Err("unable to mark user as sended", "err", err)
			continue
		}
	}
}

func (a *app) syncUnsentTasks() {
	tasks, err := a.repo.NotSendedTasks(context.TODO())
	if err != nil {
		log.Err("unable to get unsent tasks", "err", err)
		return
	}

	for _, task := range tasks {
		err := a.mngtSvc.SendTask(task)
		if err != nil {
			log.Err("unable to send task to management", "err", err)
			continue
		}

		err = a.repo.MarkTaskSended(context.TODO(), task.ID)
		if err != nil {
			log.Err("unable to mark task as sended", "err", err)
			continue
		}
	}
}

func (a *app) syncUnsentStations() {
	stations, err := a.repo.NotSendedStations(context.TODO())
	if err != nil {
		log.Err("unable to get unsent stations", "err", err)
		return
	}

	for _, station := range stations {
		err := a.mngtSvc.SendStation(station)
		if err != nil {
			log.Err("unable to send station to management", "err", err)
			continue
		}

		err = a.repo.MarkStationSended(context.TODO(), station.ID)
		if err != nil {
			log.Err("unable to mark station as sended", "err", err)
			continue
		}
	}
}

func (a *app) syncUnsentConfigs() {
	configStrings, err := a.repo.NotSendedConfigStrings(context.TODO())
	if err != nil {
		log.Err("unable to get unsent config strings", "err", err)
		return
	}
	for _, config := range configStrings {
		err := a.mngtSvc.SendConfigString(config)
		if err != nil {
			log.Err("unable to send config string to management", "err", err)
			continue
		}

		err = a.repo.MarkConfigStringSended(context.TODO(), config.Name)
		if err != nil {
			log.Err("unable to mark config string as sended", "err", err)
			continue
		}
	}

	configInts, err := a.repo.NotSendedConfigInts(context.TODO())
	if err != nil {
		log.Err("unable to get unsent config ints", "err", err)
		return
	}
	for _, config := range configInts {
		err := a.mngtSvc.SendConfigInt(config)
		if err != nil {
			log.Err("unable to send config int to management", "err", err)
			continue
		}

		err = a.repo.MarkConfigIntSended(context.TODO(), config.Name)
		if err != nil {
			log.Err("unable to mark config int as sended", "err", err)
			continue
		}
	}

	configBools, err := a.repo.NotSendedConfigBools(context.TODO())
	if err != nil {
		log.Err("unable to get unsent config bools", "err", err)
		return
	}
	for _, config := range configBools {
		err := a.mngtSvc.SendConfigBool(config)
		if err != nil {
			log.Err("unable to send config bool to management", "err", err)
			continue
		}

		err = a.repo.MarkConfigBoolSended(context.TODO(), config.Name)
		if err != nil {
			log.Err("unable to mark config bool as sended", "err", err)
			continue
		}
	}

	stationConfigStrings, err := a.repo.NotSendedStationConfigStrings(context.TODO())
	if err != nil {
		log.Err("unable to get unsent station config strings", "err", err)
		return
	}
	for _, config := range stationConfigStrings {
		err := a.mngtSvc.SendStationConfigString(config)
		if err != nil {
			log.Err("unable to send station config string to management", "err", err)
			continue
		}

		err = a.repo.MarkStationConfigStringSended(context.TODO(), config.Name, config.StationID)
		if err != nil {
			log.Err("unable to mark station config string as sended", "err", err)
			continue
		}
	}

	stationConfigBools, err := a.repo.NotSendedStationConfigBools(context.TODO())
	if err != nil {
		log.Err("unable to get unsent station config bools", "err", err)
		return
	}
	for _, config := range stationConfigBools {
		err := a.mngtSvc.SendStationConfigBool(config)
		if err != nil {
			log.Err("unable to send station config bool to management", "err", err)
			continue
		}

		err = a.repo.MarkStationConfigBoolSended(context.TODO(), config.Name, config.StationID)
		if err != nil {
			log.Err("unable to mark station config bool as sended", "err", err)
			continue
		}
	}

	stationConfigInts, err := a.repo.NotSendedStationConfigInts(context.TODO())
	if err != nil {
		log.Err("unable to get unsent station config ints", "err", err)
		return
	}
	for _, config := range stationConfigInts {
		err := a.mngtSvc.SendStationConfigInt(config)
		if err != nil {
			log.Err("unable to send station config int to management", "err", err)
			continue
		}

		err = a.repo.MarkStationConfigIntSended(context.TODO(), config.Name, config.StationID)
		if err != nil {
			log.Err("unable to mark station config int as sended", "err", err)
			continue
		}
	}
}
