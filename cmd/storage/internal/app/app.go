package app

import (
	"errors"
)

// Errors.
var (
	ErrNotFound = errors.New("not found")
)

type (
	// App is an application interface.
	App interface {
		Save(hash string, key string, value []byte) error
		Load(hash string, key string) ([]byte, error)
		
		Info() string
		
		GetServiceMoneyByHash(hash string) (int, error)
		GetServiceMoneyById(id string) (int, error)
		GetIdByHash(hash string) (string, error)
		
		SetServiceMoneyById(id string, money int) error
		
		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(hash string) (MoneyReport, error)
		LoadRelayReport(hash string) (RelayReport, error)
		
		PairIdAndHash(id string, hash string) error
	}
	
	// Repo is a DAL interface.
	Repo interface {
		Save(stationID string, key string, value []byte) error
		Load(stationID string, key string) ([]byte, error)
		Info() string
	}
)

type app struct {
	repo    Repo
	washMap map[string]WashData
}

// New creates and returns new App.
func New(repo Repo) App {
	return &app{
		repo: repo,
		washMap: make(map[string]WashData)
	}
}
