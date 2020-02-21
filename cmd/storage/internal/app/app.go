package app

import (
	"errors"
	"sync"
)

// Errors.
var (
	ErrNotFound = errors.New("not found")
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
		
		SetServiceAmount(hash string, money int) error
		GetServiceAmount(hash string) int 
		
		GetId(hash string) (int, error)
		
		Set(hash string, station StationData) error
		Get(hash string) (error, StationData)
		
		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(hash string) (MoneyReport, error)
		LoadRelayReport(hash string) (RelayReport, error)
		
		PairIdAndHash(id int, hash string) error

		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id int) error

	}
	
	// Repo is a DAL interface.
	Repo interface {
		Save(stationID int, key string, value []byte) error
		Load(stationID int, key string) ([]byte, error)
		Info() string
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
)

type app struct {
	repo     Repo
	stations map[string]StationData
	mutex 	 sync.Mutex
	kasseSvc KasseSvc
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc) App {
	return &app{
		repo: repo,
		stations: make(map[string]StationData),
		kasseSvc: kasseSvc,
	}
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
	LcwInfo     string
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
