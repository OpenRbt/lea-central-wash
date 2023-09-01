package payment

// PayRequest ...
type PayRequest struct {
	Amount     int64  `json:"amount"`
	ServerID   string `json:"server_id"`
	PostID     string `json:"post_id"`
	ServiceKey string `json:"service_key"`
	OrderID    string `json:"order_id"`
}

// PayResponse ...
type PayResponse struct {
	PostID  string `json:"post_id"`
	OrderID string `json:"order_id"`
	UrlPay  string `json:"url_pay"`
}

// PayСancellationRequest ...
type PayСancellationRequest struct {
	ServerID   string `json:"server_id"`
	PostID     string `json:"post_id"`
	ServiceKey string `json:"service_key"`
	OrderID    string `json:"order_id"`
}

// PayNotifcation ...
type PayNotifcation struct {
	ServerID string `json:"server_id"`
	PostID   string `json:"post_id"`
	OrderID  string `json:"order_id"`
	Status   string `json:"status"`
}

// PayError ...
type PayError struct {
	ServerID  string `json:"server_id"`
	PostID    string `json:"post_id"`
	OrderID   string `json:"order_id"`
	ErrorCode int64  `json:"error_code"`
	ErrorDesc string `json:"error_desc"`
}
