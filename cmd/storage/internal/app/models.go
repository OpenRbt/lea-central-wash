package app

import "time"

type UserData struct {
	ID         int
	Login      string
	FirstName  string
	MiddleName string
	LastName   string
	Password   string
	IsAdmin    bool
	IsEngineer bool
	IsOperator bool
}

type UpdateUserData struct {
	Login      *string
	FirstName  *string
	MiddleName *string
	LastName   *string
	IsAdmin    *bool
	IsEngineer *bool
	IsOperator *bool
}

type UpdatePasswordData struct {
	Login       string
	OldPassword string
	NewPassword string
}

type StationData struct {
	ID             StationID
	Name           string
	ServiceMoney   int
	LastPing       time.Time
	OpenStation    bool
	CurrentBalance int
	CurrentProgram int
}

type MoneyReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
}

type CollectionReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Ctime        time.Time
}

type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

type RelayReport struct {
	StationID  StationID
	RelayStats []RelayStat
}

type CardReaderConfig struct {
	StationID      StationID
	CardReaderType string
	Host           string
	Port           string
}
