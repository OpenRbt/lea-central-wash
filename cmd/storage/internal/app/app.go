package app

import (
	"errors"
	"sync"
	"time"
)

const durationStationOffline = time.Second * 10

// Errors.
var (
	ErrNotFound     = errors.New("not found")
	ErrAccessDenied = errors.New("access denied")
)

type (
	// App is an application interface.
	App interface {
		// Key-value methods
		Save(hash string, key string, value []byte) error
		Load(hash string, key string) ([]byte, error)

		// DBMS info method
		Info() string

		AddServiceAmount(hash string, money int) error

		GetId(hash string) (int, error)
		Set(hash string, station StationData) error
		Get(hash string) (StationData, error)
		Ping(hash string) int

		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(hash string) (*MoneyReport, error)
		LoadRelayReport(hash string) (*RelayReport, error)

		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id int) error
	}

	// Repo is a DAL interface.
	Repo interface {
		Save(stationID int, key string, value []byte) error
		Load(stationID int, key string) ([]byte, error)
		Info() string
		SetStation(station SetStation) error
		Stations() (stations []SetStation, err error)
		DelStation(id int) error
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
)

type app struct {
	repo           Repo
	stations       map[string]StationData
	stationsMutex  sync.Mutex
	kasseSvc       KasseSvc
	stationsNoHash []StationData
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc) App {
	appl := &app{
		repo:     repo,
		stations: make(map[string]StationData),
		kasseSvc: kasseSvc,
	}
	appl.loadStations()
	return appl
}

// Status describes station or kasse status.
type Status int

// Status.
const (
	StatusOffline Status = 1
	StatusOnline  Status = 2
)

type StatusReport struct {
	KasseInfo   string
	KasseStatus Status
	LCWInfo     string
	Stations    []StationStatus
}

type StationStatus struct {
	Hash   string
	ID     int
	Info   string
	Name   string
	Status Status
}

type SetStation struct {
	Hash string
	ID   int
	Name string
}
