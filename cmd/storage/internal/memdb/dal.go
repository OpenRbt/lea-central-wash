package memdb

import (
	"context"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
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

var _ = app.Repo(&DB{})

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

func (t *DB) User(login string) (user app.User, err error) {
	return app.User{}, nil
}

func (t *DB) Users(ctx context.Context, filter app.UserFilter) (users []app.User, count int64, err error) {
	return nil, 0, nil
}

func (t *DB) CreateUser(userData app.UserCreation) (newUser app.User, err error) {
	return app.User{}, nil
}

func (t *DB) UpdateUser(login string, userData app.UserUpdate) (newUser app.User, err error) {
	return app.User{}, nil
}

func (t *DB) UpdateUserPassword(login string, userData app.UpdatePasswordData) (newUser app.User, err error) {
	return app.User{}, nil
}

func (t *DB) DeleteUser(ctx context.Context, login string) (app.User, error) {
	return app.User{}, nil
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

func (t *DB) GetPrograms(ctx context.Context, filter app.ProgramFilter) ([]app.Program, int64, error) {
	return nil, 0, nil
}
func (t *DB) SetProgram(ctx context.Context, program app.Program) (app.Program, error) {
	return app.Program{}, nil
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

func (t *DB) AddAdvertisingCampaign(ctx context.Context, a app.AdvertisingCampaign) (app.AdvertisingCampaign, error) {
	return app.AdvertisingCampaign{}, nil
}

func (t *DB) EditAdvertisingCampaign(ctx context.Context, a app.AdvertisingCampaign) (app.AdvertisingCampaign, error) {
	return app.AdvertisingCampaign{}, nil
}

func (t *DB) DeleteAdvertisingCampaign(ctx context.Context, id int64) (app.AdvertisingCampaign, error) {
	return app.AdvertisingCampaign{}, nil
}

func (t *DB) GetAdvertisingCampaignByID(ctx context.Context, id int64) (app.AdvertisingCampaign, error) {
	return app.AdvertisingCampaign{}, nil
}

func (t *DB) GetAdvertisingCampaigns(ctx context.Context, filter app.AdvertisingCampaignFilter) ([]app.AdvertisingCampaign, int64, error) {
	return nil, 0, nil
}

func (t *DB) GetCurrentAdvertisingCampaigns(time.Time) ([]app.AdvertisingCampaign, error) {
	return nil, nil
}
func (t *DB) GetConfigInt(name string) (app.ConfigInt, error) {
	return app.ConfigInt{}, nil
}
func (t *DB) GetConfigBool(name string) (app.ConfigBool, error) {
	return app.ConfigBool{}, nil
}
func (t *DB) GetConfigString(name string) (app.ConfigString, error) {
	return app.ConfigString{}, nil
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

func (t *DB) GetStationConfigInt(name string, stationID app.StationID) (app.StationConfigVar[int64], error) {
	return app.StationConfigVar[int64]{}, nil
}
func (t *DB) GetStationConfigBool(name string, stationID app.StationID) (app.StationConfigVar[bool], error) {
	return app.StationConfigVar[bool]{}, nil
}
func (t *DB) GetStationConfigString(name string, stationID app.StationID) (app.StationConfigVar[string], error) {
	return app.StationConfigVar[string]{}, nil
}

func (t *DB) SetStationConfigInt(config app.StationConfigVar[int64]) error {
	return nil
}
func (t *DB) SetStationConfigBool(config app.StationConfigVar[bool]) error {
	return nil
}
func (t *DB) SetStationConfigString(config app.StationConfigVar[string]) error {
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

func (t *DB) RefreshMotorStatsCurrent() (err error) {
	return nil
}

func (t *DB) RefreshMotorStatsDates() (err error) {
	return nil
}

func (t *DB) DeleteConfigString(name string) error {
	return nil
}

func (t *DB) Collections() ([]app.CollectionReport, error) {
	return nil, nil
}
func (t *DB) CollectionSetSended(int) error {
	return nil
}
func (t *DB) MoneyReports() ([]app.MngtMoneyReport, error) {
	return nil, nil
}

func (t *DB) MoneyReportSetSended(int) error {
	return nil
}
func (t *DB) SetProgramFromManagement(ctx context.Context, program app.ManagementProgram) (app.Program, error) {
	return app.Program{}, nil
}
func (t *DB) NotSendedPrograms(ctx context.Context) ([]app.Program, error) {
	return nil, nil
}
func (t *DB) MarkProgramSended(ctx context.Context, id int64) error {
	return nil
}
func (t *DB) UpsertAdvertisingCampaignFromManagement(ctx context.Context, campaign app.ManagementAdvertisingCampaign) (app.AdvertisingCampaign, error) {
	return app.AdvertisingCampaign{}, nil
}
func (t *DB) NotSendedAdvertisingCampaigns(ctx context.Context) ([]app.AdvertisingCampaign, error) {
	return nil, nil
}
func (t *DB) MarkAdvertisingCampaignSended(ctx context.Context, id int64) error {
	return nil
}

func (r *DB) GetListBuildScripts() ([]app.BuildScript, error) {
	return nil, nil
}

func (r *DB) GetBuildScript(id int) (app.BuildScript, error) {
	return app.BuildScript{}, nil
}

func (r *DB) GetBuildScriptByStationID(app.StationID) (app.BuildScript, error) {
	return app.BuildScript{}, nil
}

func (r *DB) CreateBuildScript(createBuildScript app.SetBuildScript) (app.BuildScript, error) {
	return app.BuildScript{}, nil
}

func (r *DB) UpdateBuildScript(id int, updateBuildScript app.SetBuildScript) (app.BuildScript, error) {
	return app.BuildScript{}, nil
}

func (r *DB) UpdateBuildScriptByStationID(updateBuildScript app.SetBuildScript) (app.BuildScript, error) {
	return app.BuildScript{}, nil
}

func (r *DB) GetTask(id int) (app.Task, error) {
	return app.Task{}, nil
}

func (r *DB) GetListTasks(app.TaskFilter) ([]app.Task, int64, error) {
	return nil, 0, nil
}

func (r *DB) CreateTask(createTask app.CreateTask) (app.Task, error) {
	return app.Task{}, nil
}

func (r *DB) UpdateTask(id int, updateTask app.UpdateTask) (app.Task, error) {
	return app.Task{}, nil
}

func (r *DB) DeleteTask(id int) error {
	return nil
}

func (r *DB) CreateOpenwashingLog(model app.OpenwashingLogCreate) (app.OpenwashingLog, error) {
	return app.OpenwashingLog{}, nil
}

func (r *DB) NotSendedOpenwashingLogs(ctx context.Context) ([]app.OpenwashingLog, error) {
	return nil, nil
}

func (r *DB) MarkOpenwashingLogSended(ctx context.Context, id int64) error {
	return nil
}

func (r *DB) NotSendedConfigInts(ctx context.Context) ([]app.ConfigInt, error) {
	return nil, nil
}

func (r *DB) MarkConfigIntSended(ctx context.Context, name string) error {
	return nil
}

func (r *DB) NotSendedConfigBools(ctx context.Context) ([]app.ConfigBool, error) {
	return nil, nil
}

func (r *DB) MarkConfigBoolSended(ctx context.Context, name string) error {
	return nil
}

func (r *DB) NotSendedConfigStrings(ctx context.Context) ([]app.ConfigString, error) {
	return nil, nil
}

func (r *DB) MarkConfigStringSended(ctx context.Context, name string) error {
	return nil
}

func (r *DB) NotSendedStationConfigStrings(ctx context.Context) ([]app.StationConfigVar[string], error) {
	return nil, nil
}

func (r *DB) MarkStationConfigStringSended(ctx context.Context, name string, stationID app.StationID) error {
	return nil
}

func (r *DB) NotSendedStationConfigBools(ctx context.Context) ([]app.StationConfigVar[bool], error) {
	return nil, nil
}

func (r *DB) MarkStationConfigBoolSended(ctx context.Context, name string, stationID app.StationID) error {
	return nil
}

func (r *DB) NotSendedStationConfigInts(ctx context.Context) ([]app.StationConfigVar[int64], error) {
	return nil, nil
}

func (r *DB) MarkStationConfigIntSended(ctx context.Context, name string, stationID app.StationID) error {
	return nil
}

func (r *DB) NotSendedUsers(ctx context.Context) ([]app.User, error) {
	return nil, nil
}

func (r *DB) MarkUserSended(ctx context.Context, login string) error {
	return nil
}

func (r *DB) NotSendedTasks(ctx context.Context) ([]app.Task, error) {
	return nil, nil
}

func (r *DB) MarkTaskSended(ctx context.Context, id int) error {
	return nil
}

func (r *DB) NotSendedStations(ctx context.Context) ([]app.StationConfig, error) {
	return nil, nil
}

func (r *DB) MarkStationSended(ctx context.Context, id app.StationID) error {
	return nil
}

func (r *DB) StationUpVersion(ctx context.Context, id app.StationID) error {
	return nil
}

func (r *DB) StationUpdate(context.Context, app.StationID, app.StationUpdate) (app.StationConfig, error) {
	return app.StationConfig{}, nil
}
