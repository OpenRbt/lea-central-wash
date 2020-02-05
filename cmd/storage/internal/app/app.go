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
		Save(stationID string, key string, value []byte) error
		Load(stationID string, key string) ([]byte, error)
		Info() string
	}
	// Repo is a DAL interface.
	Repo interface {
		Save(stationID string, key string, value []byte) error
		Load(stationID string, key string) ([]byte, error)
		Info() string
	}
)

type app struct {
	repo Repo
}

// New creates and returns new App.
func New(repo Repo) App {
	return &app{
		repo: repo,
	}
}
