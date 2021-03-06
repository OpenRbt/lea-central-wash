package app

import (
	"errors"
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
)

const durationStationOffline = time.Second * 10

// For testing purposes
const (
	TestHash      = "TEST"
	TestStationID = 999
)

// Auth describes user profile.
type Auth = storageapi.Profile

// Key aliases
const (
	TemperatureCurrent = "curr_temp"
	MeteoInfo          = "meteoinfo"
	OpenWeather        = "openWeather"
	Ipify              = "ipify"
	relayTimeoutSec    = 5
)

// Errors.
var (
	ErrNotFound                 = errors.New("not found")
	ErrAccessDenied             = errors.New("access denied")
	ErrLoginNotUnique           = errors.New("login is already in use")
	ErrMoneyCollectionFkey      = errors.New("violates foreign key constraint on table money_collection")
	ErrUnknownProgram           = errors.New("unknown program")
	ErrUnknownStation           = errors.New("unknown station")
	ErrStationProgramMustUnique = errors.New("programID and buttonID must be unique")
)

type (
	// App is an application interface.
	App interface {
		// Key-value methods
		Save(stationID StationID, key string, value string) error
		SaveIfNotExists(stationID StationID, key string, value string) error
		Load(stationID StationID, key string) (string, error)
		StationsVariables() ([]StationsVariables, error)

		// DBMS info method
		Info() string

		AddServiceAmount(stationID StationID, money int) error
		OpenStation(StationID) error

		Set(station StationData) error
		Get(stationID StationID) (StationData, error)
		Ping(id StationID, balance, program int, stationIP string) StationData

		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(StationID) (*MoneyReport, error)
		LoadRelayReport(StationID) (*RelayReport, error)

		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id StationID) error
		StationReportDates(id StationID, startDate, endDate time.Time) (MoneyReport, RelayReport, error)
		StationReportCurrentMoney(id StationID) (MoneyReport, RelayReport, error)
		CollectionReports(id StationID, startDate, endDate *time.Time) (reports []CollectionReportWithUser, err error)

		StatusCollection() StatusCollection
		SaveCollectionReport(auth *Auth, id StationID) error

		Programs(id *int64) ([]Program, error)
		SetProgram(Program) error
		StationProgram(StationID) ([]StationProgram, error)
		SetStationProgram(StationID, []StationProgram) error
		StationConfig(StationID) (StationConfig, error)

		Users(auth *Auth) (users []UserData, err error)
		User(password string) (user *UserData, err error)
		CreateUser(userData UserData, auth *Auth) (id int, err error)
		UpdateUser(userData UpdateUserData, auth *Auth) (id int, err error)
		UpdateUserPassword(userData UpdatePasswordData, auth *Auth) (id int, err error)
		DeleteUser(login string, auth *Auth) error

		IsEnabled(user *UserData) bool
		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(CardReaderConfig) error

		RunProgram(id StationID, programID int64, preflight bool) (err error)
		PressButton(id StationID, buttonID int64) (err error)

		Station(StationID) (SetStation, error)
	}

	// Repo is a DAL interface.
	Repo interface {
		Save(stationID StationID, key string, value string) error
		SaveIfNotExists(stationID StationID, key string, value string) error
		Load(stationID StationID, key string) (string, error)
		Info() string
		SetStation(station SetStation) error
		Stations() (stations []SetStation, err error)
		DelStation(StationID) error
		LastMoneyReport(stationID StationID) (MoneyReport, error)
		SaveMoneyReport(MoneyReport) error
		LastRelayReport(stationID StationID) (RelayReport, error)
		SaveRelayReport(RelayReport) error
		MoneyReport(stationID StationID, startDate, endDate time.Time) (MoneyReport, error)
		RelayStatReport(stationID StationID, startDate, endDate time.Time) (RelayReport, error)
		LastCollectionReport(stationID StationID) (report CollectionReport, err error)
		SaveCollectionReport(userID int, stationID StationID) (err error)
		CollectionReports(id StationID, startDate, endDate *time.Time) (reports []CollectionReportWithUser, err error)
		StationsVariables() ([]StationsVariables, error)
		AddStation(name string) error
		AddOpenStationLog(StationID) error
		CurrentMoney(StationID) (MoneyReport, error)

		User(login string) (user UserData, err error)
		Users() (users []UserData, err error)
		CreateUser(userData UserData) (newUser UserData, err error)
		UpdateUser(userData UserData) (newUser UserData, err error)
		UpdateUserPassword(userData UpdatePasswordData) (newUser UserData, err error)
		DeleteUser(login string) error

		// for api
		LoadHash() ([]StationID, []string, error)
		SetHash(StationID, string) error
		CheckDB() (ok bool, err error)

		Programs(id *int64) ([]Program, error)
		SetProgram(Program) error
		StationProgram(StationID) ([]StationProgram, error)
		SetStationProgram(StationID, []StationProgram) error
		StationConfig(StationID) (StationConfig, error)
		Station(StationID) (SetStation, error)

		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(CardReaderConfig) error
		AddUpdateConfig(note string) (int, error)
		LastUpdateConfig() (int, error)		
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
	// WeatherSvc is an interface for the weather service
	WeatherSvc interface {
		CurrentTemperature() (float64, error)
	}
	// HardwareAccessLayer describes an interface to access hardware control modules
	HardwareAccessLayer interface {
		Start()
		ControlBoard(key int) (ControlBoard, error)
		RunProgram(id int, config RelayConfig) (err error)
	}
	// ControlBoard represents one board (even virtual) to control relays
	ControlBoard interface {
		StopAll() error
		MyPosition() (int, error)
		RunConfig(config RelayConfig)
	}
	// RelayConfig represents a relay config for something
	RelayConfig struct {
		// MotorSpeedPercent  will be passed to ESQ500/600 or another frequency controller to change the motor speed
		// NOT MORE THAN 150 PERCENT!!!
		MotorSpeedPercent int
		// If anything happens with the whole system, the control board will stop all after this time
		// NOT MORE THAN 3600 SECONDS!!!
		TimeoutSec int
		// Timings are settigns for actual relays
		Timings []Relay
	}
)

type app struct {
	repo          Repo
	stations      map[StationID]StationData
	stationsMutex sync.Mutex
	programs      map[int64]Program
	programsMutex sync.Mutex
	kasseSvc      KasseSvc
	weatherSvc    WeatherSvc
	hardware      HardwareAccessLayer
	lastUpdate    int
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc, weatherSvc WeatherSvc, hardware HardwareAccessLayer) App {
	appl := &app{
		repo:       repo,
		stations:   make(map[StationID]StationData),
		kasseSvc:   kasseSvc,
		weatherSvc: weatherSvc,
		hardware:   hardware,
	}
	appl.loadStations()
	appl.loadPrograms()
	id, err := appl.repo.LastUpdateConfig()
	if err != nil {
		log.PrintErr(err)
		id = 1
	}
	appl.stationsMutex.Lock()
	appl.lastUpdate = id
	appl.stationsMutex.Unlock()
	return appl
}

// Status describes station or kasse status.
type Status int

// StationID car wash station number
type StationID int

// Status.
const (
	StatusOffline Status = 1
	StatusOnline  Status = 2
)

// StatusCollection is a report about how much money were in a station
type StatusCollection struct {
	Stations []CollectionReport
}

// StatusReport is just a status information
type StatusReport struct {
	KasseInfo   string
	KasseStatus Status
	LCWInfo     string
	Stations    []StationStatus
}

// StationStatus is used to display in the managment software
type StationStatus struct {
	ID             StationID
	Info           string
	Name           string
	Status         Status
	CurrentBalance int
	CurrentProgram int
	ProgramName    string
	IP             string
}

// SetStation is a struct to assign a name
type SetStation struct {
	ID           StationID
	Name         string
	PreflightSec int
	RelayBoard   string
}

// StationsVariables represents a named variable for a specific Station
type StationsVariables struct {
	ID      StationID
	Name    string
	KeyPair []KeyPair
}

// KeyPair is just a Key and its value
type KeyPair struct {
	Key   string
	Value string
}

// Relay is a config for a relay
type Relay struct {
	ID      int
	TimeOn  int
	TimeOff int
}

// Program represents a program like Wax or Water or whatever ...
type Program struct {
	ID                         int64
	ButtonID                   int
	Price                      int
	Name                       string
	PreflightEnabled           bool
	MotorSpeedPercent          int64
	PreflightMotorSpeedPercent int64
	Relays                     []Relay
	PreflightRelays            []Relay
}

type StationProgram struct {
	ButtonID  int
	ProgramID int
}

// Kasse is about connected Kasse device
type Kasse struct {
	ReceiptItem     string
	TaxType         string
	CashierFullName string
	CashierINN      string
}

type StationConfig struct {
	ID           StationID
	Name         string
	PreflightSec int
	RelayBoard   string
	LastUpdate   int
	Programs     []Program
}
