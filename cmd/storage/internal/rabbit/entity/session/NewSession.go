package session

type NewSession struct {
	ID     string `json:"id,omitempty"`
	PostID int64  `json:"post_id,omitempty"`
}
