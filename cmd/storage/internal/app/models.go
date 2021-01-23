package app

import "time"

// StationData ... needs to be clarified
type StationData struct {
	ID           int
	Name         string
	ServiceMoney int
	LastPing     time.Time
}

// MoneyReport is something the station sends to the server
type MoneyReport struct {
	Hash         string
	StationID    int
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Ctime        time.Time
}

// CollectionReport is something to report
type CollectionReport struct {
	StationID int
	Money     int
	Ctime     time.Time
}

// RelayStat is a statistic for a single relay
type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

// RelayReport contains information about all relays
type RelayReport struct {
	Hash       string
	StationID  int
	RelayStats []RelayStat
}
