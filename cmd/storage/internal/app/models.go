package app

import "time"

// UserData describes a user of the system (a registered one)
type UserData struct {
	ID         int
	Login      string
	Password   string
	FirstName  *string
	MiddleName *string
	LastName   *string
	IsAdmin    *bool
	IsEngineer *bool
	IsOperator *bool
}

// UpdateUserData describes which data fields can be updated
type UpdateUserData struct {
	Login      string
	FirstName  *string
	MiddleName *string
	LastName   *string
	IsAdmin    *bool
	IsEngineer *bool
	IsOperator *bool
}

// UpdatePasswordData is a structure used in "change pass" procedure
type UpdatePasswordData struct {
	Login       string
	OldPassword string
	NewPassword string
}

// StationData represents current status of a station
type StationData struct {
	ID             StationID
	Name           string
	ServiceMoney   int
	LastPing       time.Time
	OpenStation    bool
	CurrentBalance int
	CurrentProgram int
	ButtonID       int
	LastUpdate     int
}

// MoneyReport is just to represent money in a station. All known kinds of money
type MoneyReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
}

// CollectionReport is how much was collected from a station + when
type CollectionReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Ctime        time.Time
}

// CollectionReportWithUser is how much was collected from a station + when with username who committed it
type CollectionReportWithUser struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Ctime        time.Time
	User         string
}

// RelayStat is not used now, but generally shows how much do they work
type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

// RelayReport is RelayStat for multiple(actaully all) relays of one station
type RelayReport struct {
	StationID  StationID
	RelayStats []RelayStat
}

// CardReaderConfig is for Vendotek(Ethernet) or Paymentworld(their own binary exec)
type CardReaderConfig struct {
	StationID      StationID
	CardReaderType string
	Host           string
	Port           string
}
