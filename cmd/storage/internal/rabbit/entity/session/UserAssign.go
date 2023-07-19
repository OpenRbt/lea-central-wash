package session

type UserAssign struct {
	SessionID string `json:"session_id,omitempty"`
	UserID    string `json:"user_id,omitempty"`
	Post      int64  `json:"post,omitempty"`
}
