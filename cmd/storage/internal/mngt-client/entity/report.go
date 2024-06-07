package mngt_entity

import "time"

type AddMoneyReport struct {
	WashServerID       string
	MessageID          string
	CollectionReportID string
	IsLastCollection   bool
	StationID          int
	Banknotes          int
	CarsTotal          int
	Coins              int
	Electronical       int
	Service            int
	Bonuses            int
	QrMoney            int
	Ctime              time.Time
}

type AddCollectionReport struct {
	WashServerID string
	ID           string
	StationID    int
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
	QrMoney      int
	Ctime        time.Time
}

// Status describes station or kasse status.
type Status string

// Status.
const (
	StatusOffline Status = "offline"
	StatusOnline  Status = "online"
)

// StatusReport is just a status information
type StatusReport struct {
	JustTurnedOn bool
	WashServerID string
	KasseInfo    string
	KasseStatus  Status
	LCWInfo      string
	Stations     []StationStatus
	BonusStatus  ServiceStatus
	SbpStatus    ServiceStatus
	MngtStatus   ServiceStatus
}

// StationStatus is used to display in the management software
type StationStatus struct {
	ID             int
	Info           string
	Name           string
	Status         Status
	CurrentBalance int
	CurrentProgram int
	ProgramName    string
	IP             string
}

type ServiceStatus struct {
	Available        bool
	DisabledOnServer bool
	IsConnected      bool
	LastErr          string
	DateLastErr      *time.Time
	UnpaidStations   map[int]bool
	ReconnectCount   int64
}
