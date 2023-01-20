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
	ID                 StationID
	SessionID          string
	UserID             string
	Name               string
	ServiceMoney       int
	BonusMoney         int
	LastPing           time.Time
	RunProgram         time.Time
	OpenStation        bool
	CurrentBalance     int
	CurrentProgram     int
	ButtonID           int
	LastUpdate         int
	LastDiscountUpdate int64
	IP                 string
}

// MoneyReport is just to represent money in a station. All known kinds of money
type MoneyReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
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
	ProgramID  int
	TimeOn     int
	PumpTimeOn int
	RelayStats []RelayStat
}

type StationStat struct {
	StationID    StationID
	PumpTimeOn   int
	RelayStats   []RelayStat
	ProgramStats []ProgramStat
}

type ProgramStat struct {
	ProgramID   int
	ProgramName string
	TimeOn      int
}

type StationsStat map[StationID]StationStat

// CardReaderConfig is for Vendotek(Ethernet) or Paymentworld(their own binary exec)
type CardReaderConfig struct {
	StationID      StationID
	CardReaderType string
	Host           string
	Port           string
}

type DiscountProgram struct {
	Discount  int64
	ProgramID int64
}

type AdvertisingCampaign struct {
	DefaultDiscount  int64
	DiscountPrograms []DiscountProgram
	EndDate          time.Time
	EndMinute        int64
	ID               int64
	StartDate        time.Time
	StartMinute      int64
	Weekday          []string
	Enabled          bool
	Name             string
}

type ProgramsDiscount struct {
	DefaultDiscount int64
	Discounts       map[int64]int64
}

type ButtonDiscount struct {
	ButtonID int64
	Discount int64
}

type StationDiscount struct {
	Discounts []ButtonDiscount
}

type ConfigInt struct {
	Name        string
	Value       int64
	Description string
	Note        string
}
type ConfigBool struct {
	Name        string
	Value       bool
	Description string
	Note        string
}
type ConfigString struct {
	Name        string
	Value       string
	Description string
	Note        string
}

type StationConfigInt struct {
	Name        string
	Value       int64
	Description string
	Note        string
	StationID   StationID
}
type StationConfigBool struct {
	Name        string
	Value       bool
	Description string
	Note        string
	StationID   StationID
}
type StationConfigString struct {
	Name        string
	Value       string
	Description string
	Note        string
	StationID   StationID
}
