package app

import "time"

type StationData struct {
	ID           int
	Name         string
	ServiceMoney int
	LastPing     time.Time
}

type MoneyReport struct {
	Hash         string
	StationID    int
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
}

type CollectionReport struct {
	ID    int
	Money int
	Ctime time.Time
}

type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

type RelayReport struct {
	Hash       string
	StationID  int
	RelayStats []RelayStat
}
