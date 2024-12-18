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
	case errors.Is(err, app.ErrLoginNotUnique):
		return &RPCError{
			Code:    409,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrPasswordNotUnique):
		return &RPCError{
			Code:    410,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrWrongPassword):
		return &RPCError{
			Code:    411,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrRemovingOnlyAdmin):
		return &RPCError{
			Code:    412,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrWrongParameter):
		return &RPCError{
			Code:    413,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrTaskStarted):
		return &RPCError{
			Code:    414,
			Message: err.Error(),
		}
	case errors.Is(err, app.ErrStationDirectoryNotExist):
		return &RPCError{
			Code:    415,
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

type Page[T any] struct {
	Items      []T   `json:"items"`
	Page       int64 `json:"page"`
	PageSize   int64 `json:"pageSize"`
	TotalPages int64 `json:"totalPages"`
	TotalCount int64 `json:"totalCount"`
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
	ManagementUserMessageType                app.RabbitMessageType = "management/user"
	ManagementTaskMessageType                app.RabbitMessageType = "management/task"
	ManagementStationMessageType             app.RabbitMessageType = "management/station"

	LcwProgramSettingMessageType     app.RabbitMessageType = "lcw/program/set"
	LcwProgramSettingsGetMessageType app.RabbitMessageType = "lcw/program/get"

	ServiceStatusMessageType        app.RabbitMessageType = "management/service_status"
	ServiceStatusRequestMessageType app.RabbitMessageType = "management/service_status_request"

	LcwAdvertisingCampaignsGetMessageType     app.RabbitMessageType = "lcw/advertising_campaign/get"
	LcwAdvertisingCampaignsGetByIDMessageType app.RabbitMessageType = "lcw/advertising_campaign/get_by_id"
	LcwAdvertisingCampaignCreationMessageType app.RabbitMessageType = "lcw/advertising_campaign/create"
	LcwAdvertisingCampaignUpdateMessageType   app.RabbitMessageType = "lcw/advertising_campaign/update"
	LcwAdvertisingCampaignDeletionMessageType app.RabbitMessageType = "lcw/advertising_campaign/delete"

	LcwUsersGetMessageType            app.RabbitMessageType = "lcw/user/get"
	LcwUsersGetByIDMessageType        app.RabbitMessageType = "lcw/user/get_by_id"
	LcwUsersCreationMessageType       app.RabbitMessageType = "lcw/user/create"
	LcwUsersUpdateMessageType         app.RabbitMessageType = "lcw/user/update"
	LcwUsersChangePasswordMessageType app.RabbitMessageType = "lcw/user/change_password"
	LcwUsersDeleteMessageType         app.RabbitMessageType = "lcw/user/delete"

	LcwTasksGetMessageType      app.RabbitMessageType = "lcw/task/get"
	LcwTasksGetByIDMessageType  app.RabbitMessageType = "lcw/task/get_by_id"
	LcwTasksCreationMessageType app.RabbitMessageType = "lcw/task/create"

	LcwCopyBufferedFirmwareMessageType app.RabbitMessageType = "lcw/firmware/copy"
	LcwGetBufferedFirmwareMessageType  app.RabbitMessageType = "lcw/firmware/buffered"
	LcwGetFirmwaresMessageType         app.RabbitMessageType = "lcw/firmware/get"

	LcwStationUpdateMessageType  app.RabbitMessageType = "lcw/station/update"
	LcwStationGetByIDMessageType app.RabbitMessageType = "lcw/station/get_by_id"

	LcwAddServiceAmountMessageType app.RabbitMessageType = "lcw/money/add_service_amount"
)
