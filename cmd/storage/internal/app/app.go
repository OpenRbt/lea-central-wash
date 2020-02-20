package app

import (
	"errors"
)

// Errors.
var (
	ErrNotFound     = errors.New("not found")
	ErrAccessDenied = errors.New("access denied")
)

type (
	// App is an application interface.
	App interface {
		Save(stationID string, key string, value []byte) error
		Load(stationID string, key string) ([]byte, error)
		Info() string
		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id int) error
	}
	// Repo is a DAL interface.
	Repo interface {
		Save(stationID string, key string, value []byte) error
		Load(stationID string, key string) ([]byte, error)
		Info() string
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
)

type app struct {
	repo     Repo
	kasseSvc KasseSvc
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc) App {
	return &app{
		repo:     repo,
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
