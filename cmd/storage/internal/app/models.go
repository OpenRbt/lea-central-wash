package app

type WashData struct {
	Hash         string
	Name         string
	ServiceMoney int
}

type MoneyReport struct {
	Hash         string
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
}

type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

type RelayReport struct {
	Hash       string
	RelayStats []RelayStat
}
