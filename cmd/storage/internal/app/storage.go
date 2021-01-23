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
func (a *app) Save(hash string, key string, value string) error {
	stationID, err := a.FindStationIDByHash(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", hash, "id", stationID)
		return ErrNotFound
	}

	return a.repo.Save(stationID, key, value)
}

// Load accepts key and returns value from DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Load(hash string, key string) (string, error) {
	stationID, err := a.FindStationIDByHash(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", hash, "id", stationID)
		return "", ErrNotFound
	}

	return a.repo.Load(stationID, key)
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
	stations := map[string]StationData{}
	noHash := []StationData{}

	// Calculate how many stations
	createCount := 12 - len(res)
	currentID := 1

	log.Info("CreateCount", "count", createCount)

	for createCount > 0 {
		err = a.repo.SetStation(SetStationParams{
			ID:   0,
			Hash: "",
			Name: "Station" + strconv.Itoa(currentID),
		})
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
		if res[i].Hash != "" {
			stations[res[i].Hash] = StationData{
				ID:   res[i].ID,
				Name: res[i].Name,
			}
		} else {
			noHash = append(noHash, StationData{
				ID:   res[i].ID,
				Name: res[i].Name,
			})
		}
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	a.stationsNoHash = noHash
	a.stations = stations

	return nil
}

// SaveStationData accepts existing hash and writes specified StationData
func (a *app) SaveStationData(hash string, station StationData) error {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	if value, exist := a.stations[hash]; exist {
		value = station
		a.stations[hash] = value
	} else {
		return ErrNotFound
	}
	return nil
}

// Ping sets the time of the last ping and returns service money.
func (a *app) Ping(hash string) int {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	var station StationData
	if v, ok := a.stations[hash]; ok {
		station = v
	} else {
		station = StationData{}
	}
	station.LastPing = time.Now()
	serviceMoney := station.ServiceMoney
	station.ServiceMoney = 0
	a.stations[hash] = station
	return serviceMoney
}

// StationDataByHash accepts exising hash and returns StationData
func (a *app) StationDataByHash(hash string) (StationData, error) {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()

	// TODO: if error - return empty StationData, not nil
	value, exist := a.stations[hash]
	if !exist {
		return StationData{}, ErrNotFound
	}
	return value, nil
}

// SetServiceAmount changes service money in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) AddServiceAmount(hash string, money int) error {
	data, err := a.StationDataByHash(hash)
	if err != nil && data.ID < 1 {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	data.ServiceMoney = data.ServiceMoney + money
	err = a.SaveStationData(hash, data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}
	return nil
}

// FindStationIDByHash finds ID by hash
// Returns ErrNotFound, if hash is not valid, else nil
func (a *app) FindStationIDByHash(hash string) (int, error) {
	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	station, ok := a.stations[hash]
	if !ok {
		return -1, ErrNotFound
	}
	return station.ID, nil
}

// SaveMoneyReport gets app.MoneyReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveMoneyReport(report MoneyReport) error {
	fmt.Println("-----------------------------------------SAVE MONEY REPORT ------------------")
	stationID, err := a.FindStationIDByHash(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", report.Hash, "id", stationID)
		return ErrNotFound
	}
	fmt.Printf("station id found: %+v\n", stationID)
	lastReport, err := a.FindLastReport(stationID)
	fmt.Printf("lastReport found: %+v\n", lastReport)
	if err != nil {
		log.Info("Last report is not found", "hash", report.Hash, "id", stationID)
		return ErrNotFound
	}
	// Sometimes station reports much less than it should report, so we calc the difference always
	reportDifference := a.FindReportDifference(stationID)
	fmt.Printf("difference found: %+v\n", reportDifference)

	updatedReport, newReportDifference := a.UpdatedReport(&report, lastReport, reportDifference)
	fmt.Printf("updatedReport, newReportDifference found: %+v,\n%+v\n", updatedReport, newReportDifference)
	a.SaveReportDifference(stationID, &newReportDifference)
	a.SaveLastReport(stationID, &updatedReport)

	report.StationID = stationID
	return a.repo.SaveMoneyReport(updatedReport)
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
	// TODO: remove race conditions here
	stationID, err := a.FindStationIDByHash(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", report.Hash, "id", stationID)
		return ErrNotFound
	}
	return a.repo.SaveRelayReport(report)
}

// LoadMoneyReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadMoneyReport(hash string) (*MoneyReport, error) {
	stationID, err := a.FindStationIDByHash(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}

	report, err := a.repo.LastMoneyReport(stationID)
	return &report, err
}

// LoadRelayReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadRelayReport(hash string) (*RelayReport, error) {
	stationID, err := a.FindStationIDByHash(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}
	report, err := a.repo.LastRelayReport(stationID)
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
	for key, v := range a.stations {
		var status Status
		if v.LastPing.Add(durationStationOffline).After(time.Now()) {
			status = StatusOnline
		} else {
			status = StatusOffline
		}
		report.Stations = append(report.Stations, StationStatus{
			Hash:   key,
			ID:     v.ID,
			Name:   v.Name,
			Status: status,
		})
	}
	for i := range a.stationsNoHash {
		report.Stations = append(report.Stations, StationStatus{
			ID:     a.stationsNoHash[i].ID,
			Name:   a.stationsNoHash[i].Name,
			Status: StatusOffline,
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

		t := time.Now()
		loc, err := time.LoadLocation("Europe/Moscow")
		t = t.In(loc)

		fmt.Println(t)
		fmt.Println(collectionTime)

		moneyReport, err := a.repo.MoneyReport(v.ID, collectionTime, time.Now())
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

func (a *app) SetStation(station SetStationParams) error {
	err := a.repo.SetStation(station)
	if err != nil {
		return err
	}
	a.loadStations()
	return nil
}

func (a *app) DelStation(id int) error {
	err := a.repo.DelStation(id)
	if err != nil {
		return err
	}
	a.loadStations()
	return nil
}

// Date time zone is UTC.
// The method searches for less than or equal to the end date and subtracts a report from it with a date less than or equal to the start date.
func (a *app) StationReport(id int, startDate, endDate time.Time) (MoneyReport, RelayReport, error) {
	report, err := a.repo.MoneyReport(id, startDate, endDate)
	if err != nil {
		return MoneyReport{}, RelayReport{}, err
	}

	stat, err := a.repo.RelayStatReport(id, startDate, endDate)

	return report, stat, err
}
