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

func (t *DB) User(login string) (user app.UserData, err error) {
	return app.UserData{}, nil
}

func (t *DB) Users() (users []app.UserData, err error) {
	return nil, nil
}

func (t *DB) CreateUser(userData app.UserData) (newUser app.UserData, err error) {
	return app.UserData{}, nil
}

func (t *DB) UpdateUser(userData app.UserData) (newUser app.UserData, err error) {
	return app.UserData{}, nil
}

func (t *DB) UpdateUserPassword(userData app.UpdatePasswordData) (newUser app.UserData, err error) {
	return app.UserData{}, nil
}

func (t *DB) DeleteUser(login string) error {
	return nil
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
func (t *DB) CollectionReports(stationID app.StationID, startDate, endDate *time.Time) (reports []app.CollectionReportWithUser, err error) {
	return nil, nil
}
func (t *DB) SaveCollectionReport(userID int, stationID app.StationID) error {
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

func (t *DB) Programs(id *int64) ([]app.Program, error) {
	return nil, nil
}
func (t *DB) SetProgram(app.Program) error {
	return nil
}
func (t *DB) StationProgram(app.StationID) ([]app.StationProgram, error) {
	return nil, nil
}
func (t *DB) SetStationProgram(app.StationID, []app.StationProgram) error {
	return nil
}
func (t *DB) StationConfig(id app.StationID) (cfg app.StationConfig, err error) {
	return
}

func (t *DB) Kasse() (kasse app.Kasse, err error) {
	return
}
func (t *DB) SetKasse(kasse app.Kasse) (err error) {
	return
}

func (t *DB) CardReaderConfig(stationID app.StationID) (cfg *app.CardReaderConfig, err error) {
	return
}

func (t *DB) SetCardReaderConfig(cfg app.CardReaderConfig) (err error) {
	return
}

func (t *DB) RunProgram(id *app.StationID, programID *int64) (err error) {
	return
}

func (t *DB) Station(id app.StationID) (station app.SetStation, err error) {
	return
}

func (t *DB) AddUpdateConfig(note string) (id int, err error) {
	return
}

func (t *DB) LastUpdateConfig() (id int, err error) {
	return
}

func (t *DB) RelayReportCurrent(id *app.StationID) (app.StationsStat, error) {
	return app.StationsStat{}, nil
}

func (t *DB) RelayReportDates(stationID *app.StationID, startDate, endDate time.Time) (app.StationsStat, error) {
	return app.StationsStat{}, nil
}

func (t *DB) ResetStationStat(stationID app.StationID) error {
	return nil
}

func (t *DB) AddAdvertisingCampaign(a app.AdvertisingCampaign) error {
	return nil
}

func (t *DB) EditAdvertisingCampaign(a app.AdvertisingCampaign) error {
	return nil
}
func (t *DB) DelAdvertisingCampaign(id int64) error {
	return nil
}
func (t *DB) AdvertisingCampaignByID(id int64) (*app.AdvertisingCampaign, error) {
	return nil, nil
}
func (t *DB) AdvertisingCampaign(startDate, endDate *time.Time) ([]app.AdvertisingCampaign, error) {
	return nil, nil
}

func (t *DB) GetCurrentAdvertisingCampaigns(time.Time) ([]app.AdvertisingCampaign, error) {
	return nil, nil
}
func (t *DB) GetConfigInt(name string) (*app.ConfigInt, error) {
	return nil, nil
}
func (t *DB) GetConfigBool(name string) (*app.ConfigBool, error) {
	return nil, nil
}
func (t *DB) GetConfigString(name string) (*app.ConfigString, error) {
	return nil, nil
}

func (t *DB) SetConfigInt(config app.ConfigInt) error {
	return nil
}
func (t *DB) SetConfigBool(config app.ConfigBool) error {
	return nil
}
func (t *DB) SetConfigString(config app.ConfigString) error {
	return nil
}
func (t *DB) SetConfigIntIfNotExists(config app.ConfigInt) error {
	return nil
}

func (t *DB) GetStationConfigInt(name string, stationID app.StationID) (*app.StationConfigInt, error) {
	return nil, nil
}
func (t *DB) GetStationConfigBool(name string, stationID app.StationID) (*app.StationConfigBool, error) {
	return nil, nil
}
func (t *DB) GetStationConfigString(name string, stationID app.StationID) (*app.StationConfigString, error) {
	return nil, nil
}

func (t *DB) SetStationConfigInt(config app.StationConfigInt) error {
	return nil
}
func (t *DB) SetStationConfigBool(config app.StationConfigBool) error {
	return nil
}
func (t *DB) SetStationConfigString(config app.StationConfigString) error {
	return nil
}

func (t *DB) AddRabbitMessage(message app.RabbitMessage) error {
	return nil
}

func (t *DB) GetUnsendedRabbitMessages(lastMessageID int64) ([]app.RabbitMessage, error) {
	return nil, nil
}

func (t *DB) MarkRabbitMessageAsSent(id int64) (err error) {
	return nil
}

func (t *DB) GetUnsendedMoneyReports(lastMessageID int64) (rabbitMoneyReports []app.RabbitMoneyReport, err error) {
	return nil, nil
}

func (t *DB) SaveMoneyReportAndMessage(report app.RabbitMoneyReport) (err error) {
	return nil
}
func (t *DB) MarkRabbitMoneyReportAsSent(id int64) (err error) {
	return nil
}
