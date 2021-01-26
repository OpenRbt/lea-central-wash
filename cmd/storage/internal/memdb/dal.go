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
	keypair []keypair
	mutex   sync.Mutex
}

// New DB
func New() *DB {
	return &DB{keypair: []keypair{}}
}

func (t *DB) LoadHash() ([]app.StationID, []string, error) {
	return nil, nil, nil
}
func (t *DB) SetHash(id app.StationID, hash string) error {
	return nil
}

func (t *DB) Load(stationID app.StationID, key string) (string, error) {
	t.mutex.Lock()
	defer t.mutex.Unlock()

	i := t.findKey(stationID, key)
	if i < 0 {
		return "", app.ErrNotFound
	}
	return t.keypair[i].Value, nil
}

func (t *DB) Save(stationID app.StationID, key string, value string) (err error) {
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

func (t *DB) SaveIfNotExists(stationID app.StationID, key string, value string) (err error) {
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
	return nil
}

func (t *DB) findKey(stationID app.StationID, key string) int {
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

func (t *DB) SetStation(station app.SetStation) error {
	return nil
}

func (t *DB) Stations() (stations []app.SetStation, err error) {
	return nil, nil
}

func (t *DB) DelStation(id app.StationID) error {
	return nil
}

func (t *DB) LastMoneyReport(stationID app.StationID) (report app.MoneyReport, err error) {
	return
}

func (t *DB) SaveMoneyReport(report app.MoneyReport) error {
	return nil
}

func (t *DB) LastRelayReport(stationID app.StationID) (report app.RelayReport, err error) {
	return
}

func (t *DB) SaveRelayReport(report app.RelayReport) error {
	return nil
}

func (t *DB) MoneyReport(stationID app.StationID, startDate, endDate time.Time) (report app.MoneyReport, err error) {
	return
}

func (t *DB) CurrentMoney(stationID app.StationID) (report app.MoneyReport, err error) {
	return
}

func (t *DB) RelayStatReport(stationID app.StationID, startDate, endDate time.Time) (report app.RelayReport, err error) {
	return
}

func (t *DB) LastCollectionReport(stationID app.StationID) (report app.CollectionReport, err error) {
	return
}

func (t *DB) SaveCollectionReport(id app.StationID) error {
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
