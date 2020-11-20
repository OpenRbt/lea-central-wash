package app

import "time"

type StationData struct {
	ID           StationID
	Name         string
	ServiceMoney int
	LastPing     time.Time
	OpenStation  bool
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
	StationID StationID
	Money     int
	Ctime     time.Time
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

type ProgramID int

type ProgramInfo struct {
	pID  ProgramID
	Name string
}

type Programs struct {
	sID      StationID
	programs []ProgramInfo
}

type SetProgramName struct {
	sID  StationID
	pID  ProgramID
	Name string
}

type ProgramRelays struct {
	sID     StationID
	pID     ProgramID
	program string
}

type SetProgramRelays struct {
	sID     StationID
	pID     ProgramID
	program string
}
