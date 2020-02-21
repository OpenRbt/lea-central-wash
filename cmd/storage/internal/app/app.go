package app

import (
	"errors"
	"sync"
)

// Errors.
var (
	ErrNotFound = errors.New("not found")
	ErrDuplicateHash = error.New("hash duplicated")
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
	}
	
	// Repo is a DAL interface.
	Repo interface {
		Save(stationID int, key string, value []byte) error
		Load(stationID int, key string) ([]byte, error)
		Info() string
	}
)

type app struct {
	repo     Repo
	stations map[string]StationData
	mutex 	 sync.Mutex
}

// New creates and returns new App.
func New(repo Repo) App {
	return &app{
		repo: repo,
		stations: make(map[string]StationData)
	}
}
