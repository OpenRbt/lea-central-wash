package payment

type ServiceStatus struct {
	IsPaid    bool `json:"isPaid"`
	IsEnabled bool `json:"isEnabled"`
}
