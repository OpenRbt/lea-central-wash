package app

import (
	"fmt"
	"sync"
	"time"

	"github.com/powerman/structlog"
)

var log = structlog.New() //nolint:gochecknoglobals

// Save accepts key-value pair and writes it to DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Save(hash string, key string, value []byte) error {
	stationID, err := a.GetId(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)
		return ErrNotFound
	}

	return a.repo.Save(stationID, key, value)
}

// Load accepts key and returns value from DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Load(hash string, key string) ([]byte, error) {
	stationID, err := a.GetId(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)
		return ErrNotFound
	}

	return a.repo.Load(stationID, key)
}

func (a *app) Info() string {
	return a.repo.Info()
}

// Set accepts existing hash and writes specified StationData
func (a *app) Set(hash string, station StationData) error {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	if value, exist := a.stations[hash]; exist {
		value = station
		a.stations[hash] = value
	} else {
		return ErrNotFound
	}
	return nil
}

// Get accepts exising hash and returns StationData
func (a *app) Get(hash string) (error, StationData) {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	// TODO: if error - return empty StationData, not nil

	if value, exist := a.stations[hash]; exist {
		return nil, value
	} else {
		return ErrNotFound, empty
	}
	return nil, empty
}

// SetServiceAmount changes service money in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) SetServiceAmount(hash string, money int) error {
	err, data := a.Get(hash)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	data.ServiceMoney = money
	err = a.Set(hash, data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return ErrNotFound
	}

	return nil
}

func (a *app) GetServiceAmount(hash string) (int, err) {
	err, data := a.Get(hash)
	if err != nil {
		log.Info("Can't get service money - station is unknown")
		return -1, ErrNotFound
	}
	result := data.ServiceMoney

	data.ServiceMoney = 0
	data.LastPing = time.Now()

	err = a.Set(hash, data)
	if err != nil {
		log.Info("Can't set service money - station is unknown")
		return -1, ErrNotFound
	}
	return result, nil
}

// GetId finds ID by hash
// Returns ErrNotFound, if hash is not valid, else nil
func (a *app) GetId(hash string) (string, error) {
	a.mutex.Lock()
	for key, value := range a.stations {
		if key == hash {
			return value.ID, nil
		}
	}
	a.mutex.Unlock()
	return -1, ErrNotFound
}

// PairIdAndHash adds a hash to the specified ID in map
// Returns ErrNotFound, if ID is not valid, else nil
func (a *app) PairIdAndHash(id int, hash string) error {
	a.mutex.Lock()
	if value, exist := a.stations[hash]; exist {
		value.ID = id
		return nil
	} else {
		return ErrNotFound
	}
	a.mutex.Unlock()
}

// SaveMoneyReport gets app.MoneyReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveMoneyReport(report MoneyReport) error {
	stationID, err := a.GetId(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", report.hash, stationID)
		return ErrNotFound
	}
	// TODO: convert to DAL money model here and save
	return nil
}

// SaveRelayReport gets app.RelayReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveRelayReport(report RelayReport) error {
	stationID, err := a.GetId(report.Hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", report.hash, stationID)
		return ErrNotFound
	}
	// TODO: convert to DAL report model here and save
	return nil
}

// LoadMoneyReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadMoneyReport(hash string) (MoneyReport, error) {
	stationID, err := a.GetId(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}

	// TODO: call DAL method to load money
	return nil, nil
}

// LoadRelayReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadRelayReport(hash string) (RelayReport, error) {
	stationID, err := a.GetId(hash)
	if err != nil {
		log.Info("Hash is not paired with the ID", hash, stationID)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}

	// TODO: call DAL method to load relays
	return nil, nil

func (a *app) StatusReport() StatusReport {
	panic("not implment")
}

func (a *app) SetStation(station SetStation) error {
	panic("not implment")
}

func (a *app) DelStation(id int) error {
	panic("not implment")
}
