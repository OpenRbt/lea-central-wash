package memdb

import (
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

type keypair struct {
	StationID app.StationID
	Key       string
	Value     string
}

type DB struct {
	keypair  []keypair
	mutex    sync.Mutex
	stations map[int]app.SetStationParams
	mReport  app.MoneyReport
	rReport  app.RelayReport
}

// New DB
func New() *DB {
	return &DB{
		keypair:  []keypair{},
		stations: make(map[int]app.SetStationParams),
	}
}

// Load loads a value for station by key
func (t *DB) Load(stationID int, key string) (string, error) {
	t.mutex.Lock()
	defer t.mutex.Unlock()

	i := t.findKey(stationID, key)
	if i < 0 {
		return "", app.ErrNotFound
	}
	return t.keypair[i].Value, nil
}

// Save just saves a value for station for key
func (t *DB) Save(stationID int, key string, value string) (err error) {
	t.mutex.Lock()
	defer t.mutex.Unlock()

	i := t.findKey(stationID, key)
	if i < 0 {
		t.keypair = append(t.keypair, keypair{
			StationID: stationID,
			Key:       key,
			Value:     value,
		})
		return nil
	}
	t.keypair[i].Value = value
	return nil
}

func (t *DB) findKey(stationID int, key string) int {
	for i := range t.keypair {
		if t.keypair[i].StationID == stationID && t.keypair[i].Key == key {
			return i
		}
	}
	return -1
}

// Info returns database information
func (t *DB) Info() string {
	return "memdb"
}

// SetStation remembers a station
func (t *DB) SetStation(station app.SetStationParams) error {
	t.stations[station.ID] = station
	return nil
}

// Stations returns all stations
func (t *DB) Stations() (stations []app.SetStationParams, err error) {
	res := make([]app.SetStationParams, 0, len(t.stations))
	for _, val := range t.stations {
		res = append(res, val)
	}
	return res, nil
}

// DelStation just removes a station
func (t *DB) DelStation(id int) error {
	delete(t.stations, id)
	return nil
}

// LastMoneyReport ..
func (t *DB) LastMoneyReport(stationID int) (report app.MoneyReport, err error) {
	return t.mReport, nil
}

// SaveMoneyReport ..
func (t *DB) SaveMoneyReport(report app.MoneyReport) error {
	t.mReport = report
	return nil
}

// LastRelayReport ..
func (t *DB) LastRelayReport(stationID int) (report app.RelayReport, err error) {
	return t.rReport, nil
}

// SaveRelayReport ...
func (t *DB) SaveRelayReport(report app.RelayReport) error {
	t.rReport = report
	return nil
}

// MoneyReport ...
func (t *DB) MoneyReport(stationID int, startDate, endDate time.Time) (report app.MoneyReport, err error) {
	return
}

// RelayStatReport ...
func (t *DB) RelayStatReport(stationID int, startDate, endDate time.Time) (report app.RelayReport, err error) {
	return
}

// LastCollectionReport ...
func (t *DB) LastCollectionReport(stationID int) (report app.CollectionReport, err error) {
	return
}

func (t *DB) SaveCollectionReport(report app.CollectionReport) error {
	return nil
}

func (t *DB) StationsVariables() ([]app.StationsVariables, error) {
	return nil, nil
}

func (t *DB) AddStation(name string) error {
	return nil
}

func (t *DB) AddOpenStationLog(id app.StationID) error {
	return nil
}

func (t *DB) CheckDB() (bool, error) {
	return true, nil
}

func (t *DB) Programs(id app.StationID) (programs []app.Program, err error) {
	return
}
func (t *DB) SetProgramName(id app.StationID, programID int, name string) (err error) {
	return
}
func (t *DB) ProgramRelays(id app.StationID, programID int) (relays []app.Relay, err error) {
	return
}
func (t *DB) SetProgramRelays(id app.StationID, programID int, relays []app.Relay) (err error) {
	return
}

func (t *DB) Kasse() (kasse app.Kasse, err error) {
	return
}
func (t *DB) SetKasse(kasse app.Kasse) (err error) {
	return
}
