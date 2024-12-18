package ping

type BonusPing struct {
	WashID   string          `json:"washId"`
	Stations []StationStatus `json:"stations"`
}

type StationStatus struct {
	ID       int  `json:"id"`
	IsOnline bool `json:"isOnline"`
}

