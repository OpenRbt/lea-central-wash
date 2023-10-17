package admin

type ServerUpdate struct {
	ID          string  `json:"id,omitempty"`
	Title       *string `json:"title,omitempty"`
	Description *string `json:"description,omitempty"`
	Deleted     *bool   `json:"deleted,omitempty"`
}
