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

type Pagination struct {
	Page     int64 `json:"page"`
	PageSize int64 `json:"pageSize"`
}

func PaginationToApp(pagination Pagination) app.Pagination {
	return app.Pagination{
		Page:     pagination.Page,
		PageSize: pagination.PageSize,
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
	ManagementConfigStringMessageType        app.RabbitMessageType = "management/config_string"
	ManagementConfigIntMessageType           app.RabbitMessageType = "management/config_int"
	ManagementConfigBoolMessageType          app.RabbitMessageType = "management/config_bool"
	ManagementStationConfigStringMessageType app.RabbitMessageType = "management/station_config_string"
	ManagementStationConfigIntMessageType    app.RabbitMessageType = "management/station_config_int"
	ManagementStationConfigBoolMessageType   app.RabbitMessageType = "management/station_config_bool"

	LcwProgramSettingMessageType     app.RabbitMessageType = "lcw/program/set"
	LcwProgramSettingsGetMessageType app.RabbitMessageType = "lcw/program/get"

	LcwAdvertisingCampaignsGetMessageType     app.RabbitMessageType = "lcw/advertising_campaign/get"
	LcwAdvertisingCampaignsGetByIDMessageType app.RabbitMessageType = "lcw/advertising_campaign/get_by_id"
	LcwAdvertisingCampaignCreationMessageType app.RabbitMessageType = "lcw/advertising_campaign/create"
	LcwAdvertisingCampaignUpdateMessageType   app.RabbitMessageType = "lcw/advertising_campaign/update"
	LcwAdvertisingCampaignDeletionMessageType app.RabbitMessageType = "lcw/advertising_campaign/delete"
)
