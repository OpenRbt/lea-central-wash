package session

type PostSessions struct {
	NewSessions []string `json:"new_sessions,omitempty"`
	PostID      int64    `json:"post_id,omitempty"`
}
