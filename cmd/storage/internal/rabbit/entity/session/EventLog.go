package session

import "time"

type EventLog struct {
	SessionID string                 `json:"session_id,omitempty"`
	Date      time.Time              `json:"date"`
	EventType int64                  `json:"event_type,omitempty"`
	Payload   map[string]interface{} `json:"payload,omitempty"`
}
