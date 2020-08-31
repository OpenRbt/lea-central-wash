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
		Save(hash string, key string, value string) error
		Load(hash string, key string) (string, error)

		// DBMS info method
		Info() string

		AddServiceAmount(hash string, money int) error

		ApplicationStationIDsManager
		ApplicationStationReportsValidator

		Ping(hash string) int

		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(hash string) (*MoneyReport, error)
		LoadRelayReport(hash string) (*RelayReport, error)

		StatusReport() StatusReport
		SetStation(station SetStation) error
		DelStation(id int) error
		StationReport(id int, startDate, endDate time.Time) (MoneyReport, RelayReport, error)

		StatusCollection() StatusCollection
		SaveCollectionReport(report CollectionReport) error
	}

	// ApplicationStationIDsManager has methods describing manipulating with IDs
	ApplicationStationIDsManager interface {
		FindStationIDByHash(hash string) (int, error)
		SaveStationData(hash string, station StationData) error
		StationDataByHash(hash string) (StationData, error)
	}

	// ApplicationStationReportsValidator requires to validate station reports, so we do not save rubbish data
	ApplicationStationReportsValidator interface {
		// FindLastReport is to find the latest report saved for a station
		FindLastReport(StationID int) (*MoneyReport, error)
		// FindReportDifference gets calculated difference. If current report is less that what saved in the database
		// we must calculate the difference and always add it,
		FindReportDifference(StationID int) *MoneyReport

		// UpdatedReport calculates the report we must save in the database, returns updatedReport and new reportDifference
		UpdatedReport(ReceivedReport, LastCachedReport, ReportDifference *MoneyReport) (FinalReport, NewReportDiff MoneyReport)

		// SaveReportDifference just saves the calculated difference in cache
		SaveReportDifference(StationID int, ReportDifference *MoneyReport)

		// SaveLastReportJust updates
		SaveLastReport(StationID int, UpdatedReport *MoneyReport)
	}

	// Repo is a DAL interface.
	Repo interface {
		Save(stationID int, key string, value string) error
		Load(stationID int, key string) (string, error)
		Info() string
		SetStation(station SetStation) error
		Stations() (stations []SetStation, err error)
		DelStation(id int) error
		LastMoneyReport(stationID int) (MoneyReport, error)
		SaveMoneyReport(MoneyReport) error
		LastRelayReport(stationID int) (RelayReport, error)
		SaveRelayReport(RelayReport) error
		MoneyReport(stationID int, startDate, endDate time.Time) (MoneyReport, error)
		RelayStatReport(stationID int, startDate, endDate time.Time) (RelayReport, error)
		LastCollectionReport(stationID int) (report CollectionReport, err error)
		SaveCollectionReport(report CollectionReport) (err error)
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
)

type app struct {
	repo               Repo
	stations           map[string]StationData
	lastReports        map[int]MoneyReport
	reportsDifferences map[int]MoneyReport
	stationsMutex      sync.Mutex
	kasseSvc           KasseSvc
	stationsNoHash     []StationData
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc) App {
	appl := &app{
		repo:               repo,
		stations:           make(map[string]StationData),
		lastReports:        make(map[int]MoneyReport),
		reportsDifferences: make(map[int]MoneyReport),
		kasseSvc:           kasseSvc,
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

// StatusCollection is to be removed, it is for inkassation report
type StatusCollection struct {
	Stations []CollectionReport
}

// StatusReport is something which need to be rebuilt...
type StatusReport struct {
	KasseInfo   string
	KasseStatus Status
	LCWInfo     string
	Stations    []StationStatus
}

// StationStatus is a complete information about a station
type StationStatus struct {
	Hash   string
	ID     int
	Info   string
	Name   string
	Status Status
}

// SetStation is a struct required to assign a new ID to a hash
type SetStation struct {
	Hash string
	ID   int
	Name string
}
