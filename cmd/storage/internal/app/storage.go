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
	stationID, err := a.GetId(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", hash, "id", stationID)
		return ErrNotFound
	}

	return a.repo.Save(stationID, key, value)
}

// Load accepts key and returns value from DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Load(hash string, key string) (string, error) {
	stationID, err := a.GetId(hash)
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
		err = a.repo.SetStation(SetStation{
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

	for i, _ := range res {
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

// Set accepts existing hash and writes specified StationData
func (a *app) Set(hash string, station StationData) error {
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

// Get accepts exising hash and returns StationData
func (a *app) Get(hash string) (StationData, error) {
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
	data, err := a.Get(hash)
	if err != nil && data.ID < 1 {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	data.ServiceMoney = data.ServiceMoney + money
	err = a.Set(hash, data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}
	return nil
}

// GetId finds ID by hash
// Returns ErrNotFound, if hash is not valid, else nil
func (a *app) GetId(hash string) (int, error) {
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
	stationID, err := a.GetId(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", report.Hash, "id", stationID)
		return ErrNotFound
	}

	report.StationID = stationID
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
	stationID, err := a.GetId(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", "hash", report.Hash, "id", stationID)
		return ErrNotFound
	}
	report.StationID = stationID
	return a.repo.SaveRelayReport(report)
}

// LoadMoneyReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadMoneyReport(hash string) (*MoneyReport, error) {
	stationID, err := a.GetId(hash)
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
	stationID, err := a.GetId(hash)
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
	for i, _ := range a.stationsNoHash {
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

		fmt.Println("Station begin")

		// if the post is new, and no collections found at this moment
		if err != nil {
			// set very old date
			collectionTime = time.Date(
				2000, 1, 1, 0, 0, 0, 0, time.UTC)
			fmt.Println("Old date set")
		} else {
			collectionTime = report.Ctime
			fmt.Println("Last date set")
		}
		fmt.Println("Now: ")
		fmt.Println(time.Now())
		fmt.Println("Collection time: ")
		fmt.Println(collectionTime)

		moneyReport, err := a.repo.MoneyReport(v.ID, time.Now(), collectionTime)
		if err != nil {
			collectionMoney = 0
			fmt.Println("Money = 0, because of error in DAL")
		} else {
			collectionMoney = moneyReport.Banknotes + moneyReport.Coins
			fmt.Print("Money calculated: ")
			fmt.Println(collectionMoney)
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
