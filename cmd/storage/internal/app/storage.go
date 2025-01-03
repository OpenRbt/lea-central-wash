package app

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"time"

	uuid "github.com/satori/go.uuid"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/ping"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/session"
	rabbitVo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"

	"github.com/powerman/structlog"
	"golang.org/x/crypto/bcrypt"
)

var log = structlog.New() //nolint:gochecknoglobals

// Save accepts key-value pair and writes it to DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Save(id StationID, key string, value string) error {
	return a.repo.Save(id, key, value)
}

// Save accepts key-value pair and if not exists writes it to DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) SaveIfNotExists(id StationID, key string, value string) error {
	return a.repo.SaveIfNotExists(id, key, value)
}

// Load accepts key and returns value from DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Load(id StationID, key string) (string, error) {
	switch key {
	case TemperatureCurrent:
		val, err := a.weatherSvc.CurrentTemperature()
		if err != nil {
			log.Info(TemperatureCurrent, "err", err)
			return "", err
		}
		return fmt.Sprintf("%f", val), nil
	default:
		return a.repo.Load(id, key)
	}
}

func (a *app) Info() string {
	return a.repo.Info()
}

func (a *app) loadStations() error {
	res, err := a.repo.Stations()
	if err != nil {
		log.Info("loadStations", "err", err)
		return err
	}
	stations := map[StationID]StationData{}
	sessionsPool := make(map[StationID]chan string)

	// Calculate how many stations
	createCount := 12 - len(res)
	currentID := 1

	log.Info("CreateCount", "count", createCount)

	for createCount > 0 {
		err = a.repo.AddStation("Station" + strconv.Itoa(currentID))
		if err != nil {
			log.Info("Error", "error", err)
			return err
		}
		createCount--
		currentID++
		log.Info("Station created. CreateCount after iteration", "count", createCount)
	}

	res, err = a.repo.Stations()
	if err != nil {
		log.Info("loadStations", "err", err)
		return err
	}

	for i := range res {
		stations[res[i].ID] = StationData{
			ID:       res[i].ID,
			Name:     res[i].Name,
			IsActive: res[i].IsActive,
		}
		sessionsPool[res[i].ID] = make(chan string, 30)
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	a.stationSessionPoolMutex.Lock()
	defer a.stationSessionPoolMutex.Unlock()
	a.stations = stations
	a.stationsSessionsPool = sessionsPool

	return nil
}

func (a *app) FetchSessions() (err error) {
	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()
	for i := range a.stations {
		err = a.RequestSessionsFromService(10, a.stations[i].ID)
		if err != nil {
			return
		}
	}
	return
}

// Set accepts existing hash and writes specified StationData
func (a *app) Set(station StationData) error {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	if _, exist := a.stations[station.ID]; exist {
		value := station
		a.stations[station.ID] = value
	} else {
		return ErrNotFound
	}
	return nil
}

// Ping sets the time of the last ping and returns service money.
func (a *app) Ping(id StationID, balance, program int, stationIP string, justTurnedOn bool) StationData {
	a.stationsMutex.RLock()
	var station StationData
	if v, ok := a.stations[id]; ok {
		station = v
	} else {
		station = StationData{}
	}
	a.stationsMutex.RUnlock()
	oldStation := station
	station.LastPing = time.Now()
	station.ServiceMoney = 0
	station.KaspiMoney = 0
	station.BonusMoney = 0
	station.OpenStation = false
	station.CurrentBalance = balance
	station.CurrentProgram = program
	station.ButtonID = 0
	station.IP = stationIP
	station.JustTurnedOn = justTurnedOn || oldStation.JustTurnedOn
	station.RunProgram = oldStation.RunProgram
	if oldStation.CurrentProgram != station.CurrentProgram {
		station.RunProgram = time.Now()
		if oldStation.CurrentProgram > 0 {
			go a.saveStationStat(id, oldStation.CurrentProgram, time.Since(oldStation.RunProgram))
		}
	}
	a.stationsMutex.Lock()
	a.stations[id] = station
	a.stationsMutex.Unlock()

	oldStation.LastUpdate = a.lastUpdate
	oldStation.LastDiscountUpdate = a.lastDiscountUpdate

	if oldStation.Versions == nil || oldStation.LastPing.Add(durationStationOffline).Before(time.Now()) {
		tasks, _, err := a.repo.GetListTasks(TaskFilter{
			StationsID: []StationID{id},
			Types:      []TaskType{GetVersionsTaskType},
			Statuses:   []TaskStatus{QueueTaskStatus, StartedTaskStatus},
		})
		if err != nil {
			log.PrintErr("Error getting list of tasks for station %d: %s", id, err.Error())
			return oldStation
		}
		if len(tasks) > 0 {
			return oldStation
		}

		_, err = a.repo.CreateTask(CreateTask{
			StationID: id,
			Type:      GetVersionsTaskType,
		})
		if err != nil {
			log.PrintErr("Error when creating a task to get versions from station %d: %s", id, err.Error())
			return oldStation
		}
	}

	return oldStation
}

func (a *app) checkStationOnline() {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	for i := range a.stations {
		if a.stations[i].CurrentProgram > 0 && time.Since(a.stations[i].LastPing).Seconds() > 5 {
			station := a.stations[i]
			station.CurrentProgram = 0
			if a.stations[i].CurrentProgram != station.CurrentProgram {
				if a.stations[i].CurrentProgram > 0 {
					go a.saveStationStat(a.stations[i].ID, a.stations[i].CurrentProgram, a.stations[i].LastPing.Sub(a.stations[i].RunProgram))
				}
			}
			a.stations[i] = station
		}
	}
}

func (a *app) runCheckStationOnline() {
	for {
		time.Sleep(5 * time.Second)
		a.checkStationOnline()
	}
}

func (a *app) refreshDiscounts() {
	if testApp {
		return
	}
	for {
		log.Debug("checkDiscounts", "time", time.Now().UTC())
		err := a.checkDiscounts(time.Now().UTC().Add(time.Minute * time.Duration(a.cfg.TimeZone.Value)))
		if err != nil {
			log.PrintErr(err)
		}
		start := time.Now().Truncate(time.Minute).Add(time.Minute)
		time.Sleep(time.Until(start))
	}
}

func (a *app) saveStationStat(id StationID, programID int, timeOn time.Duration) {
	timeSec := int(timeOn.Seconds())
	report := RelayReport{
		StationID:  id,
		ProgramID:  programID,
		TimeOn:     timeSec,
		PumpTimeOn: 0,
		RelayStats: []RelayStat{},
	}
	program, ok := a.programs[int64(programID)]
	if ok {
		if program.MotorSpeedPercent > 0 {
			report.PumpTimeOn = timeSec
		}
		for i := range program.Relays {
			if program.Relays[i].TimeOn > 0 {
				count := int(timeOn.Milliseconds() / int64((program.Relays[i].TimeOn + program.Relays[i].TimeOff)))
				if program.Relays[i].TimeOff == 0 {
					count = 1
				}
				report.RelayStats = append(report.RelayStats, RelayStat{
					RelayID:       program.Relays[i].ID,
					SwitchedCount: count,
					TotalTimeOn:   timeOn.Milliseconds() / int64((program.Relays[i].TimeOn + program.Relays[i].TimeOff)) * int64(program.Relays[i].TimeOn) / 1000,
				})
			}
		}
	} else {
		log.Warn("saveStationStat unknown program", "programID", programID)
	}
	err := a.repo.SaveRelayReport(report)
	if err != nil {
		log.Err(err)
	}
}

func (a *app) PressButton(id StationID, buttonID int64) (err error) {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	station := a.stations[id]
	station.ButtonID = int(buttonID)
	a.stations[id] = station
	return nil
}

// Get accepts exising hash and returns StationData
func (a *app) Get(id StationID) (StationData, error) {
	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()

	// TODO: if error - return empty StationData, not nil
	value, exist := a.stations[id]
	if !exist {
		return StationData{}, ErrNotFound
	}
	value.LastUpdate = a.lastUpdate
	value.LastDiscountUpdate = a.lastDiscountUpdate
	return value, nil
}

// SetServiceAmount changes service money in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) AddServiceAmount(id StationID, money int) error {
	data, err := a.Get(id)
	if err != nil && data.ID < 1 {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	data.ServiceMoney = data.ServiceMoney + money
	err = a.Set(data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}
	return nil
}

// AddKaspiAmount changes service money in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) AddKaspiAmount(id StationID, money int64) error {
	data, err := a.Get(id)
	if err != nil && data.ID < 1 {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	data.KaspiMoney = data.KaspiMoney + money
	err = a.Set(data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}
	return nil
}

// OpenStation changes open station in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) OpenStation(id StationID) error {
	data, err := a.Get(id)
	if err != nil && data.ID < 1 {
		log.Info("Can't open station - station is unknown")
		return ErrNotFound
	}

	data.OpenStation = true
	err = a.Set(data)
	if err != nil {
		log.Info("Can't open station - station is unknown")
		return ErrNotFound
	}
	err = a.repo.AddOpenStationLog(id)
	if err != nil {
		log.Info("OpenStation: error saving log", "err", err)
		return err
	}
	return nil
}

// SaveMoneyReport gets app.MoneyReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveMoneyReport(report MoneyReport) error {
	if report.SessionID != "" {
		err := a.repo.SaveMoneyReportAndMessage(RabbitMoneyReport{
			MessageType: string(rabbitVo.SessionMoneyReportMessageType),
			MoneyReport: report,
		})
		if err != nil {
			log.Err("failed to save moneyReport", "err", err)
			return err
		}

		return nil
	}

	return a.repo.SaveMoneyReport(report)
}

// SaveCollectionReport gets app.CollectionReport struct
func (a *app) SaveCollectionReport(auth *Auth, id StationID) error {
	fmt.Println("APP: SaveCollectionReport")
	return a.repo.SaveCollectionReport(auth.ID, id)
}

// SaveRelayReport gets app.RelayReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveRelayReport(report RelayReport) error {
	return a.repo.SaveRelayReport(report)
}

// LoadMoneyReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadMoneyReport(id StationID) (*MoneyReport, error) {
	report, err := a.repo.LastMoneyReport(id)
	return &report, err
}

func (a *app) Users(ctx context.Context, auth *Auth) ([]User, error) {
	if !auth.IsAdmin {
		return nil, ErrAccessDenied
	}

	users, _, err := a.repo.Users(ctx, UserFilter{})
	if err != nil {
		return nil, err
	}

	return users, nil
}

func (a *app) User(ctx context.Context, password string) (User, error) {
	users, _, errRepo := a.repo.Users(ctx, UserFilter{})
	if errRepo != nil {
		log.Info("REPO: ", errRepo)
		return User{}, errRepo
	}
	for u := range users {
		user := users[u]
		errPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(password))
		if errPassword == nil {
			log.Info("Authenticated", "login", user.Login, "isAdmin", user.IsAdmin)
			return user, nil
		}
	}
	return User{}, ErrNotFound
}

func (a *app) isPasswordUnique(ctx context.Context, password string) bool {
	_, errExists := a.User(ctx, password)
	return errExists == ErrNotFound
}

func (a *app) CreateUser(ctx context.Context, userCreation UserCreation, auth *Auth) (User, error) {
	if !auth.IsAdmin {
		return User{}, ErrAccessDenied
	}
	if !a.isPasswordUnique(ctx, userCreation.Password) {
		return User{}, ErrAccessDenied
	}

	userCreation, err := defaultUserSettings(userCreation)
	if err != nil {
		return User{}, err
	}

	newUser, err := a.repo.CreateUser(userCreation)
	if err != nil {
		return User{}, err
	}

	a.SendManagementSyncSignal()

	return newUser, nil
}

func defaultUserSettings(userCreation UserCreation) (UserCreation, error) {
	password, errPassword := bcrypt.GenerateFromPassword([]byte(userCreation.Password), bcrypt.DefaultCost)
	if errPassword != nil {
		return UserCreation{}, errPassword
	}
	userCreation.Password = string(password)
	defaultName := ""
	defaultPermission := false
	if userCreation.FirstName == nil {
		userCreation.FirstName = &defaultName
	}
	if userCreation.MiddleName == nil {
		userCreation.MiddleName = &defaultName
	}
	if userCreation.LastName == nil {
		userCreation.LastName = &defaultName
	}
	if userCreation.IsAdmin == nil {
		userCreation.IsAdmin = &defaultPermission
	}
	if userCreation.IsOperator == nil {
		userCreation.IsOperator = &defaultPermission
	}
	if userCreation.IsEngineer == nil {
		userCreation.IsEngineer = &defaultPermission
	}
	return userCreation, nil
}

func (a *app) UpdateUser(login string, userUpdate UserUpdate, auth *Auth) (User, error) {
	if !auth.IsAdmin {
		return User{}, ErrAccessDenied
	}

	_, err := a.repo.User(login)
	if err != nil {
		return User{}, err
	}

	user, err := a.repo.UpdateUser(login, userUpdate)
	if err != nil {
		return User{}, err
	}

	a.SendManagementSyncSignal()

	return user, err
}

func (a *app) UpdateUserPassword(ctx context.Context, login string, userData UpdatePasswordData, auth *Auth) (User, error) {
	if !auth.IsAdmin {
		return User{}, ErrAccessDenied
	}
	if !a.isPasswordUnique(ctx, userData.NewPassword) {
		return User{}, ErrPasswordNotUnique
	}
	user, errOldUser := a.repo.User(login)
	if errOldUser != nil {
		return User{}, ErrNotFound
	}
	errOldPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(userData.OldPassword))
	if errOldPassword != nil {
		return User{}, ErrNotFound
	}
	newPassword, errNewPassword := bcrypt.GenerateFromPassword([]byte(userData.NewPassword), bcrypt.DefaultCost)
	if errNewPassword != nil {
		return User{}, errNewPassword
	}
	newPasswordStr := string(newPassword)
	newUser, err := a.repo.UpdateUser(login, UserUpdate{NewPassword: &newPasswordStr})
	if err != nil {
		return User{}, err
	}

	a.SendManagementSyncSignal()

	return newUser, nil
}

func (a *app) DeleteUser(ctx context.Context, login string, auth *Auth) error {
	if !auth.IsAdmin {
		return ErrAccessDenied
	}

	_, err := a.repo.DeleteUser(ctx, login)
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()

	return nil
}

func (a *app) RelayReportCurrent(auth *Auth, id *StationID) (StationsStat, error) {
	return a.repo.RelayReportCurrent(id)
}

func (a *app) StatusReport(onlyActive bool, offJustTurnedOn bool) StatusReport {
	report := StatusReport{
		LCWInfo: a.repo.Info(),
	}
	k, err := a.kasseSvc.Info()
	if err == nil {
		report.KasseStatus = StatusOnline
		report.KasseInfo = k
	} else {
		report.KasseStatus = StatusOffline
	}
	if a.bonusSystemRabbitWorker != nil {
		report.BonusStatus = a.bonusSystemRabbitWorker.Status()
	} else {
		report.BonusStatus = ServiceStatus{Available: false}
	}

	if a.SbpWorker != nil {
		report.SbpStatus = a.SbpWorker.sbpBroker.Status()
	} else {
		report.SbpStatus = ServiceStatus{Available: false}
	}

	if a.mngtSvc.ManagementRabbitWorker != nil {
		report.MngtStatus = a.mngtSvc.Status()
	} else {
		report.MngtStatus = ServiceStatus{Available: false}
	}

	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()
	for k, v := range a.stations {
		if onlyActive && !v.IsActive {
			continue
		}
		var status Status
		if v.LastPing.Add(durationStationOffline).After(time.Now()) {
			status = StatusOnline
		} else {
			status = StatusOffline
		}
		a.programsMutex.RLock()
		programName := a.programs[int64(v.CurrentProgram)].Name
		a.programsMutex.RUnlock()
		report.Stations = append(report.Stations, StationStatus{ //
			ID:             v.ID,
			Name:           v.Name,
			Status:         status,
			CurrentBalance: v.CurrentBalance,
			CurrentProgram: v.CurrentProgram,
			ProgramName:    programName,
			IP:             v.IP,
			Version:        v.CurrentVersions,
			JustTurnedOn:   v.JustTurnedOn,
		})
		if v.JustTurnedOn && offJustTurnedOn {
			v.JustTurnedOn = false
			a.stations[k] = v
		}
	}
	return report
}

func (a *app) StatusCollection() StatusCollection {
	status := StatusCollection{}

	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()

	for _, v := range a.stations {
		report, err := a.repo.LastCollectionReport(v.ID)
		if err == nil {
			status.Stations = append(status.Stations, report)
		} else {
			status.Stations = append(status.Stations, CollectionReport{
				StationID: v.ID,
			})
		}
	}
	return status
}

func (a *app) SetStation(ctx context.Context, station SetStation) error {
	err := a.repo.SetStation(station)
	if err != nil {
		return err
	}
	a.loadStations()

	if a.IsBonusAvailable() {
		a.RequestSessionsFromService(10, station.ID)
	}

	err = a.updateConfig("SetStation")
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()

	return nil
}

func (a *app) DelStation(ctx context.Context, id StationID) error {
	err := a.repo.DelStation(id)
	if err != nil {
		return err
	}

	a.loadStations()
	a.SendManagementSyncSignal()

	return nil
}

// Date time zone is UTC.
// StationReportDates amount of money for the specified period.
func (a *app) StationReportDates(id StationID, startDate, endDate time.Time) (MoneyReport, RelayReport, error) {
	report, err := a.repo.MoneyReport(id, startDate, endDate)
	if err != nil {
		return MoneyReport{}, RelayReport{}, err
	}
	// stat, err := a.repo.RelayStatReport(id, startDate, endDate)

	return report, RelayReport{}, err
}

// StationReportCurrentMoney current amount of money in the station, after the last collection
func (a *app) StationReportCurrentMoney(id StationID) (MoneyReport, RelayReport, error) {
	report, err := a.repo.CurrentMoney(id)
	if err != nil {
		return MoneyReport{}, RelayReport{}, err
	}

	// stat, err := a.repo.RelayStatReport(id, time.Unix(0, 0), time.Now().UTC())

	return report, RelayReport{}, err
}
func (a *app) CollectionReports(id StationID, startDate, endDate *time.Time) (reports []CollectionReportWithUser, err error) {
	reports, err = a.repo.CollectionReports(id, startDate, endDate)

	return reports, err
}
func (a *app) StationsVariables() ([]StationsVariables, error) {
	return a.repo.StationsVariables()
}

func (a *app) Programs(id *int64) ([]Program, error) {
	programs, _, err := a.repo.GetPrograms(context.TODO(), ProgramFilter{ID: id})
	return programs, err
}
func (a *app) SetProgram(program Program) error {
	_, err := a.repo.SetProgram(context.TODO(), program)
	if err != nil {
		return err
	}
	a.programsMutex.Lock()
	a.programs[program.ID] = program
	a.programsMutex.Unlock()
	err = a.updateConfig("SetProgram")

	a.SendManagementSyncSignal()
	return err
}
func (a *app) StationProgram(id StationID) ([]StationProgram, error) {
	return a.repo.StationProgram(id)
}

func (a *app) SetStationProgram(ctx context.Context, id StationID, button []StationProgram) error {
	err := a.repo.SetStationProgram(id, button)
	if err != nil {
		return err
	}
	err = a.updateConfig("SetStationProgram")
	if err != nil {
		return err
	}

	err = a.repo.StationUpVersion(ctx, id)
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()

	return nil
}

func (a *app) StationConfig(id StationID) (StationConfig, error) {
	res, err := a.repo.StationConfig(id)
	if err != nil {
		return res, err
	}
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	res.LastUpdate = a.lastUpdate
	return res, nil
}

func (a *app) Kasse() (kasse Kasse, err error) {
	return a.repo.Kasse()
}
func (a *app) SetKasse(kasse Kasse) (err error) {
	return a.repo.SetKasse(kasse)
}

func (a *app) CardReaderConfig(stationID StationID) (*CardReaderConfig, error) {
	return a.repo.CardReaderConfig(stationID)
}
func (a *app) SetCardReaderConfig(ctx context.Context, cfg CardReaderConfig) error {
	err := a.repo.SetCardReaderConfig(cfg)
	if err != nil {
		return err
	}

	err = a.repo.StationUpVersion(ctx, cfg.StationID)
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()

	return nil
}

func (a *app) Station(id StationID) (SetStation, error) {
	return a.repo.Station(id)
}

func (a *app) updateConfig(note string) error {
	id, err := a.repo.AddUpdateConfig(note)
	if err != nil {
		return err
	}
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	a.lastUpdate = id
	return nil
}

func (a *app) loadPrograms() error {
	programs, _, err := a.repo.GetPrograms(context.TODO(), ProgramFilter{})
	if err != nil {
		return err
	}
	mapPrograms := make(map[int64]Program)
	for _, program := range programs {
		mapPrograms[program.ID] = program
	}
	a.programsMutex.Lock()
	a.programs = mapPrograms
	a.programsMutex.Unlock()
	return nil
}

func (a *app) RelayReportDates(auth *Auth, stationID *StationID, startDate, endDate time.Time) (StationsStat, error) {
	return a.repo.RelayReportDates(stationID, startDate, endDate)
}

func (a *app) ResetStationStat(auth *Auth, stationID StationID) error {
	return a.repo.ResetStationStat(stationID)
}

func (a *app) AddAdvertisingCampaign(ctx context.Context, auth *Auth, res AdvertisingCampaign) (AdvertisingCampaign, error) {
	campaign, err := a.repo.AddAdvertisingCampaign(ctx, res)
	if err != nil {
		return AdvertisingCampaign{}, err
	}

	a.SendManagementSyncSignal()
	return campaign, nil
}

func (a *app) EditAdvertisingCampaign(auth *Auth, res AdvertisingCampaign) error {
	_, err := a.repo.EditAdvertisingCampaign(context.TODO(), res)
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()
	return nil
}

func (a *app) DelAdvertisingCampaign(auth *Auth, id int64) error {
	_, err := a.repo.DeleteAdvertisingCampaign(context.TODO(), id)
	if err != nil {
		return err
	}

	a.SendManagementSyncSignal()
	return nil
}

func (a *app) AdvertisingCampaignByID(auth *Auth, id int64) (AdvertisingCampaign, error) {
	return a.repo.GetAdvertisingCampaignByID(context.TODO(), id)
}

func (a *app) AdvertisingCampaign(auth *Auth, startDate, endDate *time.Time) ([]AdvertisingCampaign, error) {
	campaigns, _, err := a.repo.GetAdvertisingCampaigns(context.TODO(), AdvertisingCampaignFilter{
		StartDate: startDate,
		EndDate:   endDate,
	})

	return campaigns, err
}

func (a *app) currentAdvertisingCampaigns(localTime time.Time) ([]AdvertisingCampaign, error) {
	campagins, err := a.repo.GetCurrentAdvertisingCampaigns(localTime)
	if err != nil {
		return nil, err
	}
	validCampagins := []AdvertisingCampaign{}
	for i := range campagins {
		if isValidPromotion(localTime, campagins[i]) {
			validCampagins = append(validCampagins, campagins[i])
		}
	}
	return validCampagins, nil
}

func (a *app) checkDiscounts(localTime time.Time) (err error) {
	campagins, err := a.currentAdvertisingCampaigns(localTime)
	if err != nil {
		return err
	}
	tmpDiscounts := ProgramsDiscount{
		DefaultDiscount: 0,
		Discounts:       make(map[int64]int64),
	}

	for _, campagin := range campagins {
		if campagin.DefaultDiscount > tmpDiscounts.DefaultDiscount {
			tmpPrograms := map[int64]int64{}
			for i, v := range tmpDiscounts.Discounts {
				if v > campagin.DefaultDiscount {
					tmpPrograms[i] = v
				}
			}
			tmpDiscounts.Discounts = tmpPrograms
		}
		for i, v := range tmpDiscounts.Discounts {
			if v < campagin.DefaultDiscount {
				tmpDiscounts.Discounts[i] = campagin.DefaultDiscount
			}
		}

		for _, discount := range campagin.DiscountPrograms {
			if val, ok := tmpDiscounts.Discounts[discount.ProgramID]; ok {
				tmpDiscounts.Discounts[discount.ProgramID] = max(val, discount.Discount, tmpDiscounts.DefaultDiscount)
			} else {
				tmpDiscounts.Discounts[discount.ProgramID] = max(discount.Discount, tmpDiscounts.DefaultDiscount)
			}
		}
		tmpDiscounts.DefaultDiscount = max(campagin.DefaultDiscount, tmpDiscounts.DefaultDiscount)
	}

	a.programsDiscountMutex.Lock()
	defer a.programsDiscountMutex.Unlock()

	if !reflect.DeepEqual(a.programsDiscounts, tmpDiscounts) {
		a.programsDiscounts = tmpDiscounts
		log.Info("Discounts updated", "Default discount", a.programsDiscounts.DefaultDiscount)
		a.lastDiscountUpdate = localTime.Unix()
	}

	return nil
}

func max(v ...int64) int64 {
	if len(v) == 0 {
		return 0
	}
	out := v[0]
	for i := range v {
		if v[i] > out {
			out = v[i]
		}
	}
	return out
}
func (a *app) GetStationDiscount(id StationID) (discount *StationDiscount, err error) {
	stationPrograms, err := a.StationProgram(id)
	if err != nil {
		return nil, err
	}

	discount = &StationDiscount{
		Discounts: []ButtonDiscount{},
	}

	a.programsDiscountMutex.Lock()

	for _, program := range stationPrograms {
		if val, ok := a.programsDiscounts.Discounts[int64(program.ProgramID)]; ok {
			discount.Discounts = append(discount.Discounts, ButtonDiscount{
				ButtonID: int64(program.ButtonID),
				Discount: val,
			})
		} else {
			discount.Discounts = append(discount.Discounts, ButtonDiscount{
				ButtonID: int64(program.ButtonID),
				Discount: a.programsDiscounts.DefaultDiscount,
			})
		}
	}

	defer a.programsDiscountMutex.Unlock()

	return
}

func isValidPromotion(timeLocal time.Time, a AdvertisingCampaign) bool {
	minute := int64(timeLocal.Hour()*60 + timeLocal.Minute())
	if a.StartMinute != 0 || a.EndMinute != 0 {
		if (a.StartMinute <= a.EndMinute) && ((a.StartMinute > minute) || (minute >= a.EndMinute)) {
			return false
		}
		if (a.StartMinute > a.EndMinute) && ((a.EndMinute <= minute) && (minute < a.StartMinute)) {
			return false
		}
	}
	dayOfWeek := timeLocal.Weekday()
	if (a.StartMinute > a.EndMinute) && (minute < a.StartMinute) {
		dayOfWeek = timeLocal.Add(-24 * time.Hour).Weekday()
	}
	return isValidDayOfWeek(int(dayOfWeek), a.Weekday)
}

func isValidDayOfWeek(dayOfWeek int, weekDay []string) bool {
	if len(weekDay) == 0 {
		return true
	}
	dayName := ""

	switch dayOfWeek {
	case 0:
		dayName = "sunday"
	case 1:
		dayName = "monday"
	case 2:
		dayName = "tuesday"
	case 3:
		dayName = "wednesday"
	case 4:
		dayName = "thursday"
	case 5:
		dayName = "friday"
	case 6:
		dayName = "saturday"
	}
	for _, v := range weekDay {
		if v == dayName {
			return true
		}
	}
	return false
}

func (a *app) StartSession(url string, stationID StationID) (string, string, error) {
	if a.bonusSystemRabbitWorker == nil {
		return "", "", ErrNoRabbitWorker
	}

	if !a.IsBonusAvailable() {
		return "", "", ErrServiceNotAvailable
	}

	err := a.SetNextSession(stationID)
	if err != nil {
		return "", "", err
	}

	a.stationsMutex.Lock()
	station := a.stations[stationID]
	sessionID := station.CurrentSessionID
	a.stationsMutex.Unlock()

	msg := session.StateChange{
		SessionID:      sessionID,
		State:          rabbitVo.SessionStateStart,
		AdditionalData: nil,
	}

	eventErr := a.PrepareRabbitMessage(string(rabbitVo.SessionStateMessageType), msg)
	if eventErr != nil {
		log.Err("failed preparing RabbitMessage for send session creation to bonus service", "error", eventErr)

		return "", "", err
	}

	return sessionID, fmt.Sprintf(qrURL, url, sessionID), nil
}

func (a *app) EndSession(stationID StationID, sessionID BonusSessionID) error {
	if a.bonusSystemRabbitWorker == nil {
		return ErrNoRabbitWorker
	}

	msg := session.StateChange{
		SessionID:      string(sessionID),
		State:          rabbitVo.SessionStateFinish,
		AdditionalData: nil,
	}
	eventErr := a.PrepareRabbitMessage(string(rabbitVo.SessionStateMessageType), msg)
	if eventErr != nil {
		log.Err("failed preparing RabbitMessage for send finish session to bonus service", "error", eventErr)
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	station := a.stations[stationID]

	endingSession := string(sessionID)

	if station.CurrentSessionID == endingSession {
		station.CurrentSessionID = ""
	}

	if station.AuthorizedSessionID == endingSession {
		station.AuthorizedSessionID = ""
		station.UserID = ""
	}

	if station.PreviousSessionID == endingSession {
		station.PreviousSessionID = ""
	}

	a.stations[stationID] = station

	return nil
}

func (a *app) SetBonuses(stationID StationID, bonuses int) error {
	if a.bonusSystemRabbitWorker == nil {
		return ErrNoRabbitWorker
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	station := a.stations[stationID]

	if station.UserID == "" {
		return ErrUserIsNotAuthorized
	}

	msg := session.BonusReward{
		SessionID: station.AuthorizedSessionID,
		Amount:    bonuses,
		UUID:      uuid.NewV4().String(),
	}

	err := a.PrepareRabbitMessage(string(rabbitVo.SessionBonusRewardMessageType), msg)
	if err != nil {
		log.Err("failed preparing RabbitMessage for reward with bonuses to bonus service", "error", err)
	}

	return err
}

func (a *app) AssignRabbitPub(publishFunc func(msg interface{}, service rabbitVo.Service, target rabbitVo.RoutingKey, messageType rabbitVo.MessageType) error) {
	a.servicesPublisherFunc = publishFunc
}

func (a *app) SetExternalServicesActive(active bool) {
	a.extServicesActive = active
}

func (a *app) GetRabbitConfig() (cfg RabbitConfig, err error) {
	serverID, err := a.repo.GetConfigString("server_id")
	if err != nil {
		log.Info("%s", err.Error())
		err = ErrServiceNotConfigured

		return cfg, err
	}

	serverKey, err := a.repo.GetConfigString("server_key")
	if err != nil {
		log.Info("%s", err.Error())
		err = ErrServiceNotConfigured

		return cfg, err
	}

	cfg.ServerID = serverID.Value
	cfg.ServerKey = serverKey.Value

	return
}

func (a *app) SetNextSession(stationID StationID) (err error) {
	if !a.IsBonusAvailable() {
		return ErrServiceNotAvailable
	}

	a.stationsMutex.Lock()

	if station, ok := a.stations[stationID]; ok {
		a.stationsMutex.Unlock()

		a.stationSessionPoolMutex.Lock()
		sessionsPool := a.stationsSessionsPool[stationID]
		sessionsCount := len(sessionsPool)
		a.stationSessionPoolMutex.Unlock()

		switch {
		case sessionsCount == 0:
			err = a.RequestSessionsFromService(10, stationID)
			if err != nil {
				return err
			}
			return ErrSessionNotFound
		case sessionsCount > 0:
			a.stationsMutex.Lock()
			station.PreviousSessionID = station.CurrentSessionID
			a.stationsMutex.Unlock()

			a.stationSessionPoolMutex.Lock()
			station.CurrentSessionID = <-sessionsPool
			a.stationSessionPoolMutex.Unlock()

			if sessionsCount >= 5 {
				a.stationsMutex.Lock()
				a.stations[stationID] = station
				a.stationsMutex.Unlock()
				return nil
			}
			fallthrough
		case sessionsCount < 5:
			err = a.RequestSessionsFromService(10, stationID)
			if err != nil {
				return err
			}
			return nil
		}
	} else {
		a.stationsMutex.Unlock()
	}

	return ErrUnknownStation
}

func (a *app) RequestSessionsFromService(count int, stationID StationID) error {
	var err error

	msg := session.RequestSessions{NewSessionsAmount: int64(count), PostID: int64(stationID)}
	err = a.SendMessage(string(rabbitVo.SessionRequestMessageType), msg)
	if errors.Is(err, ErrNoRabbitWorker) {
		log.Err("not found rabbit worker for bonus service, no sessions will retrieved", "error", err)
		err = nil
	}

	return err
}

func (a *app) RequestBonusServiceStatus() error {
	err := a.SendMessage(string(rabbitVo.ServiceStatusRequestMessageType), nil)
	if errors.Is(err, ErrNoRabbitWorker) {
		log.Err("not found rabbit worker for bonus service", "error", err)
		err = nil
	}

	return err
}

func (a *app) RequestSbpServiceStatus() error {
	return a.sbpBroker.RequestServiceStatus()
}

func (a *app) RequestManagementServiceStatus() error {
	return a.mngtSvc.RequestServiceStatus()
}

func (a *app) RequestKaspiServiceStatus() error {
	return a.kaspiSvc.RequestServiceStatus()
}

func (a *app) AddSessionsToPool(stationID StationID, sessionsIDs ...string) error {
	a.stationSessionPoolMutex.Lock()
	defer a.stationSessionPoolMutex.Unlock()

	val, ok := a.stationsSessionsPool[stationID]
	if !ok {
		return errors.New("no sessions available")
	}

	for _, session := range sessionsIDs {
		select {
		case val <- session:
		default:
			return nil
		}
	}
	return nil
}

func (a *app) AssignSessionUser(sessionID string, userID string, post StationID) error {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	data, ok := a.stations[post]
	if !ok || data.CurrentSessionID != sessionID {
		return ErrNotFound
	}

	data.UserID = userID
	data.AuthorizedSessionID = sessionID
	a.stations[post] = data

	return nil
}

func (a *app) addStationBonuses(sessionID string, amount int, post StationID) error {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	v, ok := a.stations[post]
	if ok {
		if v.AuthorizedSessionID == sessionID && time.Since(v.LastPing).Seconds() < 5 {
			v.BonusMoney += amount
			a.stations[post] = v
			return nil
		}
	}
	return ErrNotFound
}

func (a *app) AssignSessionBonuses(sessionID string, amount int, post StationID) error {
	err := a.addStationBonuses(sessionID, amount, post)
	if err != nil {
		log.Err("AssignSessionBonuses not found session or post")
		if a.bonusSystemRabbitWorker != nil {
			msg := session.BonusChargeDiscard{SessionID: sessionID, Amount: int64(amount)}
			err := a.PrepareRabbitMessage(string(rabbitVo.SessionBonusDiscardMessageType), msg)
			if err != nil {
				log.Err("failed preparing RabbitMessage for bonuses discard to bonus service", "error", err)
				return err
			}
		} else {
			log.Warn("not found rabbit worker for bonus service")
		}
	}

	if a.bonusSystemRabbitWorker != nil {
		msg := session.BonusChargeConfirm{
			SessionID: sessionID,
			Amount:    int64(amount),
		}

		err := a.PrepareRabbitMessage(string(rabbitVo.SessionBonusConfirmMessageType), msg)
		if err != nil {
			log.Err("failed preparing RabbitMessage for bonuses assign to bonus service", "error", err)
			return err
		}
	} else {
		log.Warn("not found rabbit worker for bonus service")
	}

	return nil
}

func (a *app) InitBonusRabbitWorker(routingKey string, publisherFunc func(msg interface{}, service rabbitVo.Service, target rabbitVo.RoutingKey, messageType rabbitVo.MessageType) error, status func() ServiceStatus) {
	worker := BonusRabbitWorker{
		repo:          a.repo,
		routingKey:    routingKey,
		publisherFunc: publisherFunc,
		status:        status,
	}

	a.bonusSystemRabbitWorker = &worker

	go worker.ProcessMessages()
	go worker.ProcessMoneyReports()
}

func (a *app) SaveMoneyReportAndMessage(report RabbitMoneyReport) error {
	return a.repo.SaveMoneyReportAndMessage(report)
}

func (a *app) refreshMotorStatsCurrent() {
	if testApp {
		return
	}
	for {
		startAt := time.Now().UTC()
		log.Debug("refreshing current motor stats", "time", time.Now().UTC())
		err := a.repo.RefreshMotorStatsCurrent()
		if err != nil {
			log.PrintErr(err)
		}

		completeAt := time.Now().UTC()

		err = a.repo.SetConfigInt(ConfigInt{
			Name:  parameterNameLastMotorStatsUpdate,
			Value: completeAt.Unix(),
		})
		if err != nil {
			log.PrintErr(err)
		}

		log.Debug("refreshing current motor stats complete!", "elapsed_time", completeAt.Sub(startAt))
		time.Sleep(time.Minute * 10)
	}
}

func (a *app) refreshMotorStatsDates() {
	if testApp {
		return
	}
	for {
		startAt := time.Now().UTC()
		log.Debug("refreshing available motor stats by dates", "time", time.Now().UTC())
		err := a.repo.RefreshMotorStatsDates()
		if err != nil {
			log.PrintErr(err)
		}

		completeAt := time.Now().UTC()

		err = a.repo.SetConfigInt(ConfigInt{
			Name:  parameterNameLastMotorStatsByDateUpdate,
			Value: completeAt.Unix(),
		})
		if err != nil {
			log.PrintErr(err)
		}

		log.Debug("refreshing available motor stats by dates complete!", "elapsed_time", completeAt.Sub(startAt))

		nextRefreshTime := time.Now().Truncate(time.Hour).Add(time.Hour)
		time.Sleep(time.Until(nextRefreshTime))
	}
}

func (a *app) GetServerInfo() ServerInfo {
	return ServerInfo{
		BonusServiceURL: a.cfg.BonusServiceURL,
	}
}

func (a *app) PingServices() {
	var bonusServerID string
	var sbpServerID string
	var kaspiServerID string

	bonusConfig, err := a.repo.GetConfigString("server_id")
	if err == nil {
		bonusServerID = bonusConfig.Value
	}

	sbpConfig, err := a.repo.GetConfigString("sbp_server_id")
	if err == nil {
		sbpServerID = sbpConfig.Value
	}

	kaspiConfig, err := a.repo.GetConfigString("kaspi_server_id")
	if err == nil {
		kaspiServerID = kaspiConfig.Value
	}

	for {
		currentState := []StationPingStatus{}

		a.stationsMutex.RLock()

		for _, v := range a.stations {
			if !v.IsActive {
				continue
			}
			isOnline := false
			if v.LastPing.Add(durationStationOffline).After(time.Now()) {
				isOnline = true
			}

			currentState = append(currentState, StationPingStatus{
				ID:       v.ID,
				IsOnline: isOnline,
			})
		}

		a.stationsMutex.RUnlock()

		err := a.pingBonus(bonusServerID, currentState)
		if err != nil {
			log.Err(fmt.Sprintf("Bonus service ping error: %s", err.Error()))
			continue
		}

		err = a.pingSBP(sbpServerID, currentState)
		if err != nil {
			log.Err(fmt.Sprintf("SBP service ping error %s", err.Error()))
			continue
		}

		err = a.pingKaspi(kaspiServerID, currentState)
		if err != nil {
			log.Err(fmt.Sprintf("SBP service ping error %s", err.Error()))
			continue
		}

		time.Sleep(time.Second * 15)
	}
}

func (a *app) pingBonus(serverID string, status []StationPingStatus) error {
	if !a.IsBonusAvailable() {
		return nil
	}

	p := ping.BonusPing{
		WashID:   serverID,
		Stations: stationPingStatusToRabbit(status),
	}

	return a.bonusSystemRabbitWorker.publisherFunc(p, rabbitVo.WashBonusService, rabbitVo.BonusPingRoutingKey, rabbitVo.BonusPingMessageType)
}

func (a *app) pingSBP(serverID string, status []StationPingStatus) error {
	if !a.IsSbpAvailable() {
		return nil
	}

	return a.SbpWorker.Ping(serverID, status)
}

func (a *app) pingKaspi(serverID string, status []StationPingStatus) error {
	if !a.IsKaspiAvailable() {
		return nil
	}

	return a.kaspiSvc.Ping(serverID, status)
}

func stationPingStatusToRabbit(status []StationPingStatus) []ping.StationStatus {
	l := []ping.StationStatus{}
	for _, v := range status {
		l = append(l, ping.StationStatus{
			ID:       int(v.ID),
			IsOnline: v.IsOnline,
		})
	}
	return l
}

func (a *app) AddOpenwashingLog(log OpenwashingLogCreate) (OpenwashingLog, error) {
	newLog, err := a.repo.CreateOpenwashingLog(log)
	if err != nil {
		return OpenwashingLog{}, err
	}

	a.SendManagementSyncSignal()
	return newLog, nil
}
