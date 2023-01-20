package models

import "time"

type SessionCreation struct {
	NewSessions []string `json:"new_sessions"`
}

type SessionRequest struct {
	NewSessionsAmount int64 `json:"new_sessions_amount"`
}

type SessionStateChange struct {
	SessionID      string         `json:"session_id"`
	State          int            `json:"state"`
	AdditionalData map[string]any `json:"additional_data,omitempty"`
}

type SessionUserAssign struct {
	SessionID string `json:"session_id"`
	UserID    string `json:"user_id"`
}

type SessionBonusCharge struct {
	SessionID string `json:"session_id"`
	Amount    int64  `json:"amount"`
}

type SessionEventLog struct {
	SessionID string         `json:"session_id"`
	Date      time.Time      `json:"date"`
	EventType int64          `json:"event_type"`
	Payload   map[string]any `json:"payload"`
}
