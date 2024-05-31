package mngt_entity

import (
	"errors"
	"fmt"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
)

type RPCError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

func (e *RPCError) Error() string {
	return fmt.Sprintf("Code %d: %s", e.Code, e.Message)
}

type RPCResponse struct {
	Data  interface{} `json:"data"`
	Error *RPCError   `json:"error,omitempty"`
}

func ErrorToRPCError(err error) *RPCError {
	switch {
	case errors.Is(err, app.ErrNotFound):
		return &RPCError{
			Code:    404,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrSameOrLowerVersion):
		return &RPCError{
			Code:    1000,
			Message: err.Error(),
		}
	default:
		return &RPCError{
			Code:    500,
			Message: err.Error(),
		}
	}
}

const (
	WashAdminRoutingKey             app.RabbitRoutingKey = "wash_admin"
	WashAdminServesEventsRoutingKey app.RabbitRoutingKey = "wash_admin_servers"
	WashBonusRoutingKey             app.RabbitRoutingKey = "wash_bonus"
)

const (
	AddMoneyReportMsg      app.RabbitMessageType = "management_client_service/add_money_report"
	AddCollectionReportMsg app.RabbitMessageType = "management_client_service/add_collection_report"
	WashStatus             app.RabbitMessageType = "management_client_service/wash_status"

	ManagementProgramMessageType             app.RabbitMessageType = "management/program"
	ManagementAdvertisingCampaignMessageType app.RabbitMessageType = "management/advertising_campaign"
	ManagementOpenwashingLogMessageType      app.RabbitMessageType = "management/openwashing_log"

	LcwProgramSettingMessageType              app.RabbitMessageType = "lcw/program/set"
	LeaAdvertisingCampaignCreationMessageType app.RabbitMessageType = "lcw/advertising_campaign/create"
	LeaAdvertisingCampaignUpsertMessageType   app.RabbitMessageType = "lcw/advertising_campaign/upsert"
	LeaAdvertisingCampaignDeletionMessageType app.RabbitMessageType = "lcw/advertising_campaign/delete"
)
