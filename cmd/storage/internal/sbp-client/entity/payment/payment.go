package payment

// PayRequest ...
type PayRequest struct {
	WashID  string `json:"wash_id"`
	PostID  string `json:"post_id"`
	OrderID string `json:"order_id"`
	Amount  int64  `json:"amount"`
	Version int    `json:"version"`
}

// PayResponse ...
type PayResponse struct {
	WashID  string `json:"wash_id"`
	PostID  string `json:"post_id"`
	OrderID string `json:"order_id"`
	UrlPay  string `json:"url_pay"`
	Failed  bool   `json:"failed"`
	Error   string `json:"error"`
}

// PayСancellationRequest ...
type PayСancellationRequest struct {
	WashID  string `json:"wash_id"`
	PostID  string `json:"post_id"`
	OrderID string `json:"order_id"`
	Error   string `json:"error"`
}

type PaymentConfirmationRequest struct {
	OrderID string `json:"order_id"`
}

// PayNotifcation ...
type PayNotifcation struct {
	WashID  string `json:"wash_id"`
	PostID  string `json:"post_id"`
	OrderID string `json:"order_id"`
	Status  string `json:"status"`
}
