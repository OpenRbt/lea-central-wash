package app

import (
	"errors"
	"sync"
	"time"
)

const durationStationOffline = time.Second * 10

// For testing purposes
const (
	TestHash      = "TEST"
	TestStationID = 999
)

// Key aliases
const (
	TemperatureCurrent = "curr_temp"
	MeteoInfo          = "meteoinfo"
	OpenWeather        = "openWeather"
	Ipify              = "ipify"
)

// Errors.
var (
	ErrNotFound     = errors.New("not found")
	ErrAccessDenied = errors.New("access denied")
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
		Ping(id StationID, balance, program int) StationData

		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(StationID) (*MoneyReport, error)
		LoadRelayReport(StationID) (*RelayReport, error)

		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id StationID) error
		StationReportDates(id StationID, startDate, endDate time.Time) (MoneyReport, RelayReport, error)
		StationReportCurrentMoney(id StationID) (MoneyReport, RelayReport, error)

		StatusCollection() StatusCollection
		SaveCollectionReport(StationID) error

		Programs(id StationID) (programs []Program, err error)
		SetProgramName(id StationID, programID int, name string) (err error)
		ProgramRelays(id StationID, programID int) (relays []Relay, err error)
		SetProgramRelays(id StationID, programID int, relays []Relay) (err error)

		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(CardReaderConfig) error
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
		SaveCollectionReport(id StationID) (err error)
		StationsVariables() ([]StationsVariables, error)
		AddStation(name string) error
		AddOpenStationLog(StationID) error
		CurrentMoney(StationID) (MoneyReport, error)

		// for api
		LoadHash() ([]StationID, []string, error)
		SetHash(StationID, string) error
		CheckDB() (ok bool, err error)

		Programs(id StationID) (programs []Program, err error)
		SetProgramName(id StationID, programID int, name string) (err error)
		ProgramRelays(id StationID, programID int) (relays []Relay, err error)
		SetProgramRelays(id StationID, programID int, relays []Relay) (err error)

		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(CardReaderConfig) error
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
	// WeatherSvc is an interface for the weather service
	WeatherSvc interface {
		CurrentTemperature() (float64, error)
	}
)

type app struct {
	repo          Repo
	stations      map[StationID]StationData
	stationsMutex sync.Mutex
	kasseSvc      KasseSvc
	weatherSvc    WeatherSvc
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc, weatherSvc WeatherSvc) App {
	appl := &app{
		repo:       repo,
		stations:   make(map[StationID]StationData),
		kasseSvc:   kasseSvc,
		weatherSvc: weatherSvc,
	}
	appl.loadStations()
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

type StatusCollection struct {
	Stations []CollectionReport
}

type StatusReport struct {
	KasseInfo   string
	KasseStatus Status
	LCWInfo     string
	Stations    []StationStatus
}

type StationStatus struct {
	ID             StationID
	Info           string
	Name           string
	Status         Status
	CurrentBalance int
	CurrentProgram int
}

type SetStation struct {
	ID   StationID
	Name string
}

type StationsVariables struct {
	ID      StationID
	Name    string
	KeyPair []KeyPair
}

type KeyPair struct {
	Key   string
	Value string
}

type Relay struct {
	ID        int
	TimeOn    int
	TimeOff   int
	Preflight int
}

type Program struct {
	ID   int
	Name string
}

type Kasse struct {
	ReceiptItem     string
	TaxType         string
	CashierFullName string
	CashierINN      string
}
