package session

type MoneyReport struct {
	StationID    int    `json:"station_id,omitempty"`
	Banknotes    int    `json:"banknotes,omitempty"`
	CarsTotal    int    `json:"cars_total,omitempty"`
	Coins        int    `json:"coins,omitempty"`
	Electronical int    `json:"electronical,omitempty"`
	Service      int    `json:"service,omitempty"`
	Bonuses      int    `json:"bonuses,omitempty"`
	SessionID    string `json:"session_id,omitempty"`
	UUID         string `json:"uuid,omitempty"`
}
