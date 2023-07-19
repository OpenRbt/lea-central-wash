package session

type BonusChargeDiscard struct {
	SessionID string `json:"session_id,omitempty"`
	Amount    int64  `json:"amount,omitempty"`
}
