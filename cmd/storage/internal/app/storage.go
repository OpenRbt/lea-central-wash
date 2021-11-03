package app

import (
	"fmt"
	"reflect"
	"strconv"
	"time"

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
			ID:   res[i].ID,
			Name: res[i].Name,
		}
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	a.stations = stations

	return nil
}

// Set accepts existing hash and writes specified StationData
func (a *app) Set(station StationData) error {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	if value, exist := a.stations[station.ID]; exist {
		value = station
		a.stations[station.ID] = value
	} else {
		return ErrNotFound
	}
	return nil
}

// Ping sets the time of the last ping and returns service money.
func (a *app) Ping(id StationID, balance, program int, stationIP string) StationData {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	var station StationData
	if v, ok := a.stations[id]; ok {
		station = v
	} else {
		station = StationData{}
	}
	oldStation := station
	station.LastPing = time.Now()
	station.ServiceMoney = 0
	station.OpenStation = false
	station.CurrentBalance = balance
	station.CurrentProgram = program
	station.ButtonID = 0
	station.IP = stationIP
	station.RunProgram = oldStation.RunProgram
	if oldStation.CurrentProgram != station.CurrentProgram {
		station.RunProgram = time.Now()
		if oldStation.CurrentProgram > 0 {
			go a.saveStationStat(id, oldStation.CurrentProgram, time.Now().Sub(oldStation.RunProgram))
		}
	}
	a.stations[id] = station
	oldStation.LastUpdate = a.lastUpdate
	oldStation.LastDiscountUpdate = a.lastDiscountUpdate
	return oldStation
}

func (a *app) checkStationOnline() {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	for i := range a.stations {
		if a.stations[i].CurrentProgram > 0 && time.Now().Sub(a.stations[i].LastPing).Seconds() > 5 {
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
	return
}

func (a *app) runCheckStationOnline() {
	for {
		time.Sleep(5 * time.Second)
		a.checkStationOnline()
	}
}

func (a *app) refreshDiscounts() {
	for range time.Tick(time.Minute) {
		err := a.CheckDiscounts()
		if err != nil {
			log.PrintErr(err)
		}
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
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

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

func (a *app) IsEnabled(user *UserData) bool {
	return *user.IsAdmin || *user.IsEngineer || *user.IsOperator
}

func (a *app) Users(auth *Auth) ([]UserData, error) {
	if !auth.IsAdmin {
		return nil, ErrAccessDenied
	}
	return a.repo.Users()
}

func (a *app) User(password string) (*UserData, error) {
	users, errRepo := a.repo.Users()
	if errRepo != nil {
		log.Info("REPO: ", errRepo)
		return nil, errRepo
	}
	for u := range users {
		user := users[u]
		errPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(password))
		if errPassword == nil {
			log.Info("Authenticated", "login", user.Login, "isAdmin", *user.IsAdmin)
			return &user, nil
		}
	}
	return nil, ErrNotFound
}

func (a *app) isPasswordUnique(password string) bool {
	_, errExists := a.User(password)
	return errExists == ErrNotFound
}

func (a *app) CreateUser(userData UserData, auth *Auth) (id int, err error) {
	if !auth.IsAdmin {
		return 0, ErrAccessDenied
	}
	if !a.isPasswordUnique(userData.Password) {
		return 0, ErrAccessDenied
	}
	password, errPassword := bcrypt.GenerateFromPassword([]byte(userData.Password), bcrypt.DefaultCost)
	if errPassword != nil {
		return 0, errPassword
	}
	userData.Password = string(password)
	defaultName := ""
	defaultPermission := false
	if userData.FirstName == nil {
		userData.FirstName = &defaultName
	}
	if userData.MiddleName == nil {
		userData.MiddleName = &defaultName
	}
	if userData.LastName == nil {
		userData.LastName = &defaultName
	}
	if userData.IsAdmin == nil {
		userData.IsAdmin = &defaultPermission
	}
	if userData.IsOperator == nil {
		userData.IsOperator = &defaultPermission
	}
	if userData.IsEngineer == nil {
		userData.IsEngineer = &defaultPermission
	}
	user, err := a.repo.CreateUser(userData)
	if err != nil {
		return 0, err
	}
	return user.ID, nil
}

func (a *app) UpdateUser(userData UpdateUserData, auth *Auth) (id int, err error) {
	if !auth.IsAdmin {
		return 0, ErrAccessDenied
	}
	user, errOldUser := a.repo.User(userData.Login)
	if errOldUser != nil {
		return 0, errOldUser
	}
	if userData.FirstName != nil {
		user.FirstName = userData.FirstName
	}
	if userData.MiddleName != nil {
		user.MiddleName = userData.MiddleName
	}
	if userData.LastName != nil {
		user.LastName = userData.LastName
	}
	if userData.IsAdmin != nil {
		user.IsAdmin = userData.IsAdmin
	}
	if userData.IsOperator != nil {
		user.IsOperator = userData.IsOperator
	}
	if userData.IsEngineer != nil {
		user.IsEngineer = userData.IsEngineer
	}
	newUser, err := a.repo.UpdateUser(user)
	if err != nil {
		return 0, err
	}
	return newUser.ID, err
}

func (a *app) UpdateUserPassword(userData UpdatePasswordData, auth *Auth) (id int, err error) {
	if !auth.IsAdmin {
		return 0, ErrAccessDenied
	}
	if !a.isPasswordUnique(userData.NewPassword) {
		return 0, ErrAccessDenied
	}
	user, errOldUser := a.repo.User(userData.Login)
	if errOldUser != nil {
		return 0, ErrNotFound
	}
	errOldPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(userData.OldPassword))
	if errOldPassword != nil {
		return 0, ErrNotFound
	}
	newPassword, errNewPassword := bcrypt.GenerateFromPassword([]byte(userData.NewPassword), bcrypt.DefaultCost)
	if errNewPassword != nil {
		return 0, errNewPassword
	}
	userData.NewPassword = string(newPassword)
	newUser, err := a.repo.UpdateUserPassword(userData)
	if err != nil {
		return 0, err
	}
	return newUser.ID, nil
}

func (a *app) DeleteUser(login string, auth *Auth) error {
	if !auth.IsAdmin {
		return ErrAccessDenied
	}
	return a.repo.DeleteUser(login)
}

func (a *app) RelayReportCurrent(auth *Auth, id *StationID) (StationsStat, error) {
	return a.repo.RelayReportCurrent(id)
}

func (a *app) StatusReport() StatusReport {
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

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	for _, v := range a.stations {
		var status Status
		if v.LastPing.Add(durationStationOffline).After(time.Now()) {
			status = StatusOnline
		} else {
			status = StatusOffline
		}
		a.programsMutex.Lock()
		programName := a.programs[int64(v.CurrentProgram)].Name
		a.programsMutex.Unlock()
		report.Stations = append(report.Stations, StationStatus{
			ID:             v.ID,
			Name:           v.Name,
			Status:         status,
			CurrentBalance: v.CurrentBalance,
			CurrentProgram: v.CurrentProgram,
			ProgramName:    programName,
			IP:             v.IP,
		})
	}
	return report
}

func (a *app) StatusCollection() StatusCollection {
	status := StatusCollection{}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

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

func (a *app) SetStation(station SetStation) error {
	err := a.repo.SetStation(station)
	if err != nil {
		return err
	}
	a.loadStations()
	err = a.updateConfig("SetStation")
	return err
}

func (a *app) DelStation(id StationID) error {
	err := a.repo.DelStation(id)
	if err != nil {
		return err
	}
	a.loadStations()
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
	return a.repo.Programs(id)
}
func (a *app) SetProgram(program Program) error {
	err := a.repo.SetProgram(program)
	if err != nil {
		return err
	}
	a.programsMutex.Lock()
	a.programs[program.ID] = program
	a.programsMutex.Unlock()
	err = a.updateConfig("SetProgram")
	return err
}
func (a *app) StationProgram(id StationID) ([]StationProgram, error) {
	return a.repo.StationProgram(id)
}
func (a *app) SetStationProgram(id StationID, button []StationProgram) error {
	err := a.repo.SetStationProgram(id, button)
	if err != nil {
		return err
	}
	err = a.updateConfig("SetStationProgram")
	return err
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
func (a *app) SetCardReaderConfig(cfg CardReaderConfig) error {
	return a.repo.SetCardReaderConfig(cfg)
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
	programs, err := a.repo.Programs(nil)
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

func (a *app) AddAdvertisingCampaign(auth *Auth, res AdvertisingCampaign) error {
	return a.repo.AddAdvertisingCampaign(res)
}
func (a *app) EditAdvertisingCampaign(auth *Auth, res AdvertisingCampaign) error {
	return a.repo.EditAdvertisingCampaign(res)
}
func (a *app) DelAdvertisingCampaign(auth *Auth, id int64) error {
	return a.repo.DelAdvertisingCampaign(id)
}
func (a *app) AdvertisingCampaignByID(auth *Auth, id int64) (*AdvertisingCampaign, error) {
	return a.repo.AdvertisingCampaignByID(id)
}
func (a *app) AdvertisingCampaign(auth *Auth, startDate, endDate *time.Time) ([]AdvertisingCampaign, error) {
	return a.repo.AdvertisingCampaign(startDate, endDate)
}

func (a *app) CheckDiscounts() (err error) {
	campagins, err := a.repo.GetCurrentAdvertisingCampaigns()

	if err != nil {
		return err
	}

	tmpDiscounts := ProgramsDiscount{
		DefaultDiscount: 0,
		Discounts:       make(map[int64]int64),
	}

	for _, campagin := range campagins {
		if campagin.DefaultDiscount > tmpDiscounts.DefaultDiscount {
			tmpDiscounts.DefaultDiscount = campagin.DefaultDiscount
		}
	}

	for _, campagin := range campagins {
		for _, discount := range campagin.DiscountPrograms {
			if val, ok := tmpDiscounts.Discounts[discount.ProgramID]; ok {
				if val < discount.Discount {
					tmpDiscounts.Discounts[discount.ProgramID] = discount.Discount
				}
			} else {
				if discount.Discount > tmpDiscounts.DefaultDiscount {
					tmpDiscounts.Discounts[discount.ProgramID] = discount.Discount
				} else {
					tmpDiscounts.Discounts[discount.ProgramID] = tmpDiscounts.DefaultDiscount
				}
			}
		}
	}

	a.programsDiscountMutex.Lock()
	defer a.programsDiscountMutex.Unlock()

	if !reflect.DeepEqual(a.programsDiscounts, tmpDiscounts) {
		a.programsDiscounts = tmpDiscounts
		log.Info("Discounts updated", "Default discount", a.programsDiscounts.DefaultDiscount)
		a.lastDiscountUpdate++
	}

	return nil
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
