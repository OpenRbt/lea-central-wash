package session

type RequestSessions struct {
	NewSessionsAmount int64 `json:"new_sessions_amount,omitempty"`
	PostID            int64 `json:"post_id,omitempty"`
}
