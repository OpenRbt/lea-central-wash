package app

import (
	"fmt"
	"strconv"
	"time"

	"github.com/powerman/structlog"
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
func (a *app) Ping(id StationID) StationData {
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
	a.stations[id] = station
	return oldStation
}

// Get accepts existing hash and returns StationData
func (a *app) Get(id StationID) (StationData, error) {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	// TODO: if error - return empty StationData, not nil
	value, exist := a.stations[id]
	if !exist {
		return StationData{}, ErrNotFound
	}
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

// IsZero checks if report money is zero
func (m *MoneyReport) IsZero() bool {
	if m == nil {
		return true
	}
	// real money is empty
	if m.Banknotes != 0 || m.Electronical != 0 || m.Coins != 0 {
		return false
	}
	// other stuff is empty
	if m.CarsTotal != 0 || m.Service != 0 {
		return false
	}
	return true
}

// AddUp adds up another values
func (m *MoneyReport) AddUp(AnotherMoneyReport *MoneyReport) {
	if m == nil || AnotherMoneyReport == nil {
		return
	}
	m.Banknotes += AnotherMoneyReport.Banknotes
	m.Coins += AnotherMoneyReport.Coins
	m.Electronical += AnotherMoneyReport.Electronical
	m.CarsTotal += AnotherMoneyReport.CarsTotal
	m.Service += AnotherMoneyReport.Service
}

// AlignWith returns a difference which allows adding up current report to not-negative values
func (m *MoneyReport) AlignWith(AnotherMoneyReport *MoneyReport) MoneyReport {
	difference := MoneyReport{}
	if m == nil || AnotherMoneyReport == nil {
		return difference
	}
	difference.Banknotes = howMuchToZero(m.Banknotes, AnotherMoneyReport.Banknotes)
	difference.Coins = howMuchToZero(m.Coins, AnotherMoneyReport.Coins)
	difference.Electronical = howMuchToZero(m.Electronical, AnotherMoneyReport.Electronical)
	difference.CarsTotal = howMuchToZero(m.CarsTotal, AnotherMoneyReport.CarsTotal)
	difference.Service = howMuchToZero(m.Service, AnotherMoneyReport.Service)
	return difference
}

func howMuchToZero(Source, AlignWith int) int {
	if AlignWith < 0 {
		AlignWith = 0
	}
	if AlignWith > Source {
		return AlignWith - Source
	}
	return 0
}

// FindReportDifference gets calculated difference.
// If current report is less than what is saved in the database
// we must calculate the difference and always add it,
func (a *app) FindReportDifference(id StationID) *MoneyReport {
	resultDifference, ok := a.reportsDifferences[id]
	if ok {
		return &resultDifference
	}
	return &MoneyReport{
		StationID: id,
	}
}

// UpdatedReport calculates the report we must save in the database, returns updatedReport and new reportDifference
func (a *app) UpdatedReport(ReceivedReport, LastCachedReport, ReportDifference *MoneyReport) (FinalReport, NewReportDiff MoneyReport) {
	FinalReport = *ReceivedReport
	NewReportDiff = *ReportDifference

	if !ReportDifference.IsZero() {
		FinalReport.AddUp(ReportDifference)
	}
	additionalDifference := FinalReport.AlignWith(LastCachedReport)
	if !additionalDifference.IsZero() {
		FinalReport.AddUp(&additionalDifference)
		NewReportDiff.AddUp(&additionalDifference)
	}
	return FinalReport, NewReportDiff
}

// SaveReportDifference just saves the calculated difference in cache
func (a *app) CacheReportDifference(id StationID, ReportDifference *MoneyReport) {
	a.reportsDifferences[id] = *ReportDifference
}

// SaveLastReportJust updates
func (a *app) CacheUpdatedReport(id StationID, UpdatedReport *MoneyReport) {
	a.lastReports[id] = *UpdatedReport
}

// SaveMoneyReport gets app.MoneyReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveMoneyReport(report MoneyReport) error {
	// fmt.Println("-----------------------------------------SAVE MONEY REPORT ------------------")
	// lastReport, err := a.repo.LastMoneyReport(report.StationID)
	// if err != nil {
	// 	log.Info(fmt.Sprintf("Last report is not found, StationID=%d", report.StationID))
	// 	lastReport = MoneyReport{}
	// 	// return ErrNotFound
	// } else {
	// 	log.Info(fmt.Sprintf("LastReport found: %+v\n", lastReport))
	// }
	// // Sometimes station reports much less than it should report, so we calc the difference always
	// reportDifference := a.FindReportDifference(report.StationID)
	// fmt.Printf("difference found: %+v\n", reportDifference)

	// updatedReport, newReportDifference := a.UpdatedReport(&report, &lastReport, reportDifference)
	// fmt.Printf("updatedReport, newReportDifference found: %+v,\n%+v\n", updatedReport, newReportDifference)
	// a.CacheReportDifference(report.StationID, &newReportDifference)
	// a.CacheUpdatedReport(report.StationID, &updatedReport)

	// Sometimes station reports much less than it should report, hmm...
	// How do I, as a server, know what a station should report?
	return a.repo.SaveMoneyReport(report)
}

// SaveCollectionReport gets app.CollectionReport struct
func (a *app) SaveCollectionReport(report CollectionReport) error {
	fmt.Println("APP: SaveCollectionReport")
	return a.repo.SaveCollectionReport(report)
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

// LoadRelayReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadRelayReport(id StationID) (*RelayReport, error) {
	report, err := a.repo.LastRelayReport(id)
	return &report, err
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
		report.Stations = append(report.Stations, StationStatus{
			ID:     v.ID,
			Name:   v.Name,
			Status: status,
		})
	}
	return report
}

func (a *app) StatusCollection() StatusCollection {
	status := StatusCollection{}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	for _, v := range a.stations {
		var collectionTime time.Time
		var collectionMoney int

		report, err := a.repo.LastCollectionReport(v.ID)

		// if the post is new, and no collections found at this moment
		if err != nil {
			// set very old date
			collectionTime = time.Date(
				2000, 1, 1, 0, 0, 0, 0, time.UTC)
		} else {
			collectionTime = report.Ctime
		}

		t := time.Now().UTC()

		log.Info(fmt.Sprintf("StationID=%d", v.ID))
		log.Info(fmt.Sprintf("Last collection on %s", collectionTime))
		log.Info(fmt.Sprintf("Current time is %s", t))

		moneyReport, err := a.repo.MoneyReport(v.ID, collectionTime, t)
		if err != nil {
			collectionMoney = 0
		} else {
			collectionMoney = moneyReport.Banknotes + moneyReport.Coins
		}

		status.Stations = append(status.Stations, CollectionReport{
			StationID: v.ID,
			Money:     collectionMoney,
			Ctime:     collectionTime,
		})
	}
	return status
}

func (a *app) SetStation(station SetStation) error {
	err := a.repo.SetStation(station)
	if err != nil {
		return err
	}
	a.loadStations()
	return nil
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
// The method searches for less than or equal to the end date and subtracts a report from it with a date less than or equal to the start date.
func (a *app) StationReport(id StationID, startDate, endDate time.Time) (MoneyReport, RelayReport, error) {
	report, err := a.repo.MoneyReport(id, startDate, endDate)
	if err != nil {
		return MoneyReport{}, RelayReport{}, err
	}

	stat, err := a.repo.RelayStatReport(id, startDate, endDate)

	return report, stat, err
}

func (a *app) StationsVariables() ([]StationsVariables, error) {
	return a.repo.StationsVariables()
}

func (a *app) Programs(id StationID) (programs []Program, err error) {
	return a.repo.Programs(id)
}
func (a *app) SetProgramName(id StationID, programID int, name string) (err error) {
	return a.repo.SetProgramName(id, programID, name)
}
func (a *app) ProgramRelays(id StationID, programID int) (relays []Relay, err error) {
	return a.repo.ProgramRelays(id, programID)
}
func (a *app) SetProgramRelays(id StationID, programID int, relays []Relay) (err error) {
	return a.repo.SetProgramRelays(id, programID, relays)
}

func (a *app) Kasse() (kasse Kasse, err error) {
	return a.repo.Kasse()
}
func (a *app) SetKasse(kasse Kasse) (err error) {
	return a.repo.SetKasse(kasse)
}
