package vo

type RoutingKey string

const (
	WashAdminRoutingKey             RoutingKey = "wash_admin"
	WashAdminServesEventsRoutingKey RoutingKey = "wash_admin_servers"
	WashBonusRoutingKey             RoutingKey = "wash_bonus"
)
