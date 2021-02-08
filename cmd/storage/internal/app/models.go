package app

import "time"

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
