package session

type BonusChargeConfirm struct {
	SessionID string `json:"session_id,omitempty"`
	Amount    int64  `json:"amount,omitempty"`
}
