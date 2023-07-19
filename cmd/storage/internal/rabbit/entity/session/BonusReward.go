package session

type BonusReward struct {
	SessionID string `json:"session_id,omitempty"`
	Amount    int    `json:"amount,omitempty"`
	UUID      string `json:"uuid,omitempty"`
}
