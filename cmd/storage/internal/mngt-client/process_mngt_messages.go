package sbpclient

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	mngt_entity "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/mngt-client/entity"

	amqp "github.com/rabbitmq/amqp091-go"
)

func (s *Service) ProcessMngtMessage(d amqp.Delivery) error {
	s.log.Debug("Management Message Type", "json", d.Type)
	s.log.Debug("Management Message Body", "json", string(d.Body))

	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Second)
	defer cancel()

	switch app.RabbitMessageType(d.Type) {
	case mngt_entity.LcwProgramSettingsGetMessageType:
		return s.handleLeaProgramsGetting(ctx, d)

	case mngt_entity.LcwProgramSettingMessageType:
		return s.handleLeaProgramSetting(ctx, d)

	case mngt_entity.LcwAdvertisingCampaignsGetByIDMessageType:
		return s.handleLeaAdvertisingCampaignGettingByID(ctx, d)

	case mngt_entity.LcwAdvertisingCampaignsGetMessageType:
		return s.handleLeaAdvertisingCampaignsGetting(ctx, d)

	case mngt_entity.LcwAdvertisingCampaignCreationMessageType:
		return s.handleLeaAdvertisingCampaignCreation(ctx, d)

	case mngt_entity.LcwAdvertisingCampaignUpdateMessageType:
		return s.handleLeaAdvertisingCampaignUpdate(ctx, d)

	case mngt_entity.LcwAdvertisingCampaignDeletionMessageType:
		return s.handleLeaAdvertisingCampaignDeletion(ctx, d)

	case mngt_entity.LcwUsersGetMessageType:
		return s.handleLeaUsersGetting(ctx, d)

	case mngt_entity.LcwUsersGetByIDMessageType:
		return s.handleLeaUserGettingById(ctx, d)

	case mngt_entity.LcwUsersCreationMessageType:
		return s.handleLeaUserCreation(ctx, d)

	case mngt_entity.LcwUsersUpdateMessageType:
		return s.handleLeaUserUpdate(ctx, d)

	case mngt_entity.LcwUsersChangePasswordMessageType:
		return s.handleLeaUserChangePassword(ctx, d)

	case mngt_entity.LcwUsersDeleteMessageType:
		return s.handleLeaUserDeletion(ctx, d)

	case mngt_entity.LcwAddServiceAmountMessageType:
		return s.handleLeaAddServiceAmount(ctx, d)

	case mngt_entity.LcwTasksGetMessageType:
		return s.handleLeaTasksGetting(ctx, d)

	case mngt_entity.LcwTasksGetByIDMessageType:
		return s.handleLeaTasksGettingByID(ctx, d)

	case mngt_entity.LcwTasksCreationMessageType:
		return s.handleLeaTasksCreating(ctx, d)

	case mngt_entity.LcwCopyBufferedFirmwareMessageType:
		return s.handleLeaCopyBufferedFirmware(ctx, d)

	case mngt_entity.LcwGetBufferedFirmwareMessageType:
		return s.handleLeaBufferedFirmwareGetting(ctx, d)

	case mngt_entity.LcwGetFirmwaresMessageType:
		return s.handleLeaFirmwaresGetting(ctx, d)

	case mngt_entity.LcwStationUpdateMessageType:
		return s.handleLeaStationUpdating(ctx, d)

	case mngt_entity.LcwStationGetByIDMessageType:
		return s.handleLeaStationGetting(ctx, d)

	default:
		s.log.Warn("Unknown message type:", d.Type)
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return nil
	}
}

func (s *Service) handleLeaStationUpdating(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.StationUpdate
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}
	rpcResponse := mngt_entity.RPCResponse{}
	stationUpdate, err := mngt_entity.StationUpdateToApp(args)
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
		return s.sendMessageHandleErrors(rpcResponse, d)
	}

	station, err := s.app.StationUpdateForManagement(ctx, app.StationID(args.ID), stationUpdate)
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
		return s.sendMessageHandleErrors(rpcResponse, d)
	}

	stationModel, err := mngt_entity.StationToRabbit(station)
	rpcResponse.Data = stationModel
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaStationGetting(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	rpcResponse := mngt_entity.RPCResponse{}
	station, err := s.app.StationGetForManagement(ctx, app.StationID(args.ID))
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
		return s.sendMessageHandleErrors(rpcResponse, d)
	}

	stationModel, err := mngt_entity.StationToRabbit(station)
	rpcResponse.Data = stationModel
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaTasksGetting(ctx context.Context, d amqp.Delivery) error {
	var filter mngt_entity.TaskFilter
	if err := json.Unmarshal(d.Body, &filter); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	appFilter, err := mngt_entity.TaskFilterToApp(filter)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	tasks, err := s.app.GetTasksForManagement(ctx, appFilter)
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.TaskPageToRabbit(tasks)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaTasksGettingByID(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	task, err := s.app.GetTaskByIdForManagement(ctx, args.ID)
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.TaskToRabbit(task)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaTasksCreating(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.CreateTask
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	creaeTask, err := mngt_entity.CreateTaskToApp(args)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	task, err := s.app.CreateTaskForManagement(ctx, creaeTask)
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.TaskToRabbit(task)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaCopyBufferedFirmware(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.CopyBufferedFirmware
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err := s.app.CopyFirmwareForManagement(ctx, app.StationID(args.StationID), app.StationID(args.CopyToStationID))
	rpcResponse := mngt_entity.RPCResponse{}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaFirmwaresGetting(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	tasks, err := s.app.GetVersionsForManagement(ctx, app.StationID(args.ID))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.FirmwareVersionsToRabbit(tasks, args.ID)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaBufferedFirmwareGetting(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	tasks, err := s.app.GetVersionBufferedForManagement(ctx, app.StationID(args.ID))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.FirmwareVersionToRabbit(tasks, args.ID)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaProgramsGetting(ctx context.Context, d amqp.Delivery) error {
	var filter mngt_entity.ProgramFilter
	if err := json.Unmarshal(d.Body, &filter); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	programs, err := s.app.GetProgramsForManagement(ctx, mngt_entity.ProgramFilterToApp(filter))
	rpcResponse := mngt_entity.RPCResponse{Data: programs}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaProgramSetting(ctx context.Context, d amqp.Delivery) error {
	var program mngt_entity.Program
	if err := json.Unmarshal(d.Body, &program); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	setProgram, err := s.app.SetProgramFromManagement(ctx, mngt_entity.ProgramToApp(program))
	rpcResponse := mngt_entity.RPCResponse{Data: setProgram}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	err = s.sendMessageByCorrelationID(rpcResponse, d.ReplyTo, d.CorrelationId)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err = s.app.MarkProgramSended(ctx, setProgram.ID)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	if err = d.Ack(false); err != nil {
		return err
	}

	return nil
}

func (s *Service) handleLeaAdvertisingCampaignGettingByID(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int64]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.log.Err("Failed to unmarshal message:", err)
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	campaign, err := s.app.GetAdvertisingCampaignByIDForManagement(ctx, args.ID)
	rpcResponse := mngt_entity.RPCResponse{Data: campaign}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaAdvertisingCampaignsGetting(ctx context.Context, d amqp.Delivery) error {
	var filter mngt_entity.AdvertisingCampaignFilter
	if err := json.Unmarshal(d.Body, &filter); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	campaigns, err := s.app.GetAdvertisingCampaignsForManagement(ctx, mngt_entity.AdvertisingCampaignFilterToApp(filter))
	rpcResponse := mngt_entity.RPCResponse{Data: campaigns}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaAdvertisingCampaignCreation(ctx context.Context, d amqp.Delivery) error {
	var campaign mngt_entity.AdvertisingCampaign
	if err := json.Unmarshal(d.Body, &campaign); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	createdCampaign, err := s.app.AddAdvertisingCampaignFromManagement(ctx, mngt_entity.AdvertisingCampaignToApp(campaign))
	rpcResponse := mngt_entity.RPCResponse{Data: createdCampaign}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	err = s.sendMessageByCorrelationID(rpcResponse, d.ReplyTo, d.CorrelationId)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err = s.app.MarkAdvertisingCampaignSended(ctx, createdCampaign.ID)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	if err = d.Ack(false); err != nil {
		return err
	}

	return nil
}

func (s *Service) handleLeaAdvertisingCampaignUpdate(ctx context.Context, d amqp.Delivery) error {
	var campaign mngt_entity.AdvertisingCampaign
	if err := json.Unmarshal(d.Body, &campaign); err != nil {
		s.log.Err("Failed to unmarshal message:", err)
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	updatedCampaign, err := s.app.EditAdvertisingCampaignFromManagement(ctx, mngt_entity.AdvertisingCampaignToApp(campaign))
	rpcResponse := mngt_entity.RPCResponse{Data: updatedCampaign}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	err = s.sendMessageByCorrelationID(rpcResponse, d.ReplyTo, d.CorrelationId)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err = s.app.MarkAdvertisingCampaignSended(ctx, updatedCampaign.ID)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	if err = d.Ack(false); err != nil {
		return err
	}

	return nil
}

func (s *Service) handleLeaAdvertisingCampaignDeletion(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[int64]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.log.Err("Failed to unmarshal message:", err)
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	campaign, err := s.app.DeleteAdvertisingCampaignFromManagement(ctx, args.ID)
	rpcResponse := mngt_entity.RPCResponse{Data: campaign}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	err = s.sendMessageByCorrelationID(rpcResponse, d.ReplyTo, d.CorrelationId)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err = s.app.MarkAdvertisingCampaignSended(ctx, args.ID)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	if err = d.Ack(false); err != nil {
		return err
	}

	return nil
}

func (s *Service) handleLeaAddServiceAmount(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.AddServiceAmount
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	err := s.app.AddServiceAmountForManagement(ctx, app.StationID(args.StationID), args.Amount)
	rpcResponse := mngt_entity.RPCResponse{}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUsersGetting(ctx context.Context, d amqp.Delivery) error {
	var filter mngt_entity.UserFilter
	if err := json.Unmarshal(d.Body, &filter); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	users, err := s.app.GetUsersForManagement(ctx, mngt_entity.UserFilterToApp(filter))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserPageToRabbit(users)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUserGettingById(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[string]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	user, err := s.app.GetUserForManagement(ctx, args.ID)
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserToRabbit(user)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUserCreation(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.UserCreation
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	user, err := s.app.CreateUsersForManagement(ctx, mngt_entity.UserCreationToApp(args))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserToRabbit(user)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUserDeletion(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ArgID[string]
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	user, err := s.app.DeleteUsersForManagement(ctx, args.ID)
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserToRabbit(user)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUserUpdate(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.UserUpdate
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	user, err := s.app.UpdateUsersForManagement(ctx, args.Login, mngt_entity.UserUpdateToApp(args))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserToRabbit(user)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) handleLeaUserChangePassword(ctx context.Context, d amqp.Delivery) error {
	var args mngt_entity.ChangePassword
	if err := json.Unmarshal(d.Body, &args); err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	user, err := s.app.ChangeUserPasswordForManagement(ctx, args.Login, mngt_entity.ChangePasswordToApp(args))
	rpcResponse := mngt_entity.RPCResponse{Data: mngt_entity.UserToRabbit(user)}
	if err != nil {
		s.setLastErr(err.Error())
		rpcResponse.Error = mngt_entity.ErrorToRPCError(err)
	}

	return s.sendMessageHandleErrors(rpcResponse, d)
}

func (s *Service) SendMoneyReport(report app.MngtMoneyReport) (err error) {
	err = s.sendMessage(mngt_entity.AddMoneyReport{
		WashServerID:       s.serverID,
		MessageID:          report.ManagementMessageID.String(),
		CollectionReportID: report.CollectionReportID.String(),
		IsLastCollection:   report.IsLastCollection(),
		StationID:          int(report.StationID),
		Banknotes:          report.Banknotes,
		CarsTotal:          report.CarsTotal,
		Coins:              report.Coins,
		Electronical:       report.Electronical,
		Service:            report.Service,
		Bonuses:            report.Bonuses,
		QrMoney:            report.QrMoney,
		Ctime:              report.Ctime,
	}, mngt_entity.AddMoneyReportMsg)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendCollectionReport(report app.CollectionReport) (err error) {
	err = s.sendMessage(mngt_entity.AddCollectionReport{
		WashServerID: s.serverID,
		ID:           report.ManagementID.String(),
		StationID:    int(report.StationID),
		Banknotes:    report.Banknotes,
		CarsTotal:    report.CarsTotal,
		Coins:        report.Coins,
		Electronical: report.Electronical,
		Service:      report.Service,
		Bonuses:      report.Bonuses,
		QrMoney:      report.QrMoney,
		Ctime:        report.Ctime,
	}, mngt_entity.AddCollectionReportMsg)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStatus(report app.StatusReport, justTurnedOn bool) (err error) {
	err = s.sendMessage(s.msgStatusReport(report, justTurnedOn), mngt_entity.WashStatus)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendProgram(program app.Program) error {
	err := s.sendMessage(mngt_entity.ProgramToRabbit(program), mngt_entity.ManagementProgramMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendOpenwashingLog(log app.OpenwashingLog) error {
	err := s.sendMessage(mngt_entity.OpenwashingLogToRabbit(log), mngt_entity.ManagementOpenwashingLogMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendAdvertisingCampaign(campaign app.AdvertisingCampaign) error {
	err := s.sendMessage(mngt_entity.AdvertisingCampaignToRabbit(campaign), mngt_entity.ManagementAdvertisingCampaignMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendConfigString(config app.ConfigString) error {
	err := s.sendMessage(mngt_entity.ConfigStringToRabbit(config), mngt_entity.ManagementConfigStringMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendConfigInt(config app.ConfigInt) error {
	err := s.sendMessage(mngt_entity.ConfigIntToRabbit(config), mngt_entity.ManagementConfigIntMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendConfigBool(config app.ConfigBool) error {
	err := s.sendMessage(mngt_entity.ConfigBoolToRabbit(config), mngt_entity.ManagementConfigBoolMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStationConfigBool(config app.StationConfigVar[bool]) error {
	err := s.sendMessage(mngt_entity.StationConfigBoolToRabbit(config), mngt_entity.ManagementStationConfigBoolMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStationConfigInt(config app.StationConfigVar[int64]) error {
	err := s.sendMessage(mngt_entity.StationConfigIntToRabbit(config), mngt_entity.ManagementStationConfigIntMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStationConfigString(config app.StationConfigVar[string]) error {
	err := s.sendMessage(mngt_entity.StationConfigStringToRabbit(config), mngt_entity.ManagementStationConfigStringMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendUser(user app.User) error {
	err := s.sendMessage(mngt_entity.UserToRabbit(user), mngt_entity.ManagementUserMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendTask(task app.Task) error {
	err := s.sendMessage(mngt_entity.TaskToRabbit(task), mngt_entity.ManagementTaskMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStation(station app.StationConfig) error {
	stationModel, err := mngt_entity.StationToRabbit(station)
	if err != nil {
		s.setLastErr(err.Error())
		return err
	}
	err = s.sendMessage(stationModel, mngt_entity.ManagementStationMessageType)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) sendMessage(msg interface{}, messageType app.RabbitMessageType) (err error) {
	jsonMsg, ok := msg.([]byte)
	if !ok {
		jsonMsg, err = json.Marshal(msg)
		if err != nil {
			return
		}
	}

	message := amqp.Publishing{}
	message.Body = jsonMsg
	message.Type = messageType.String()
	message.UserId = s.serverID
	message.DeliveryMode = amqp.Persistent

	exchangeName := exchange
	routingKeyString := "management_service_client"
	dConfirmation, err := s.mngtClientPub.PublishWithDeferredConfirmWithContext(
		context.Background(),
		exchangeName,
		routingKeyString,
		false,
		false,
		message,
	)
	if err != nil {
		return err
	}

	select {
	case <-time.After(time.Second * 10):
		return app.ErrSendTimeout
	case <-dConfirmation.Done():
		if !dConfirmation.Acked() {
			return app.ErrNotConfirmed
		}

		fmt.Println("management: delivered deferred confirm to handler")
	}

	return nil
}

func (s *Service) sendMessageHandleErrors(msg interface{}, d amqp.Delivery) error {
	err := s.sendMessageByCorrelationID(msg, d.ReplyTo, d.CorrelationId)
	if err != nil {
		s.setLastErr(err.Error())
		if nackErr := d.Nack(false, false); nackErr != nil {
			return nackErr
		}
		return err
	}

	if err = d.Ack(false); err != nil {
		return err
	}

	return nil
}

func (s *Service) sendMessageByCorrelationID(msg interface{}, replyTo string, correlationID string) error {
	var jsonMsg []byte
	var err error

	switch m := msg.(type) {
	case []byte:
		jsonMsg = m
	default:
		jsonMsg, err = json.Marshal(msg)
		if err != nil {
			return fmt.Errorf("error marshaling message: %w", err)
		}
	}

	message := amqp.Publishing{
		ContentType:   "application/json",
		Body:          jsonMsg,
		UserId:        s.serverID,
		DeliveryMode:  amqp.Persistent,
		CorrelationId: correlationID,
		ReplyTo:       replyTo,
	}

	dConfirmation, err := s.mngtClientPub.PublishWithDeferredConfirmWithContext(
		context.Background(),
		exchange,
		replyTo,
		false,
		false,
		message,
	)
	if err != nil {
		return fmt.Errorf("failed to publish message: %w", err)
	}

	select {
	case <-time.After(5 * time.Second):
		return app.ErrSendTimeout
	case <-dConfirmation.Done():
		if !dConfirmation.Acked() {
			return app.ErrNotConfirmed
		}

		fmt.Println("Management: Delivered deferred confirm to handler")
	}

	return nil
}

func (s *Service) msgStatusReport(v app.StatusReport, justTurnedOn bool) mngt_entity.StatusReport {
	var stationStatus []mngt_entity.StationStatus
	for i := range v.Stations {
		stationStatus = append(stationStatus, msgStationStatus(v.Stations[i]))
	}

	return mngt_entity.StatusReport{
		JustTurnedOn: justTurnedOn,
		KasseInfo:    v.KasseInfo,
		KasseStatus:  msgStatus(v.KasseStatus),
		LCWInfo:      v.LCWInfo,
		Stations:     stationStatus,
		SbpStatus:    msgServiceStatus(v.SbpStatus),
		BonusStatus:  msgServiceStatus(v.BonusStatus),
		MngtStatus:   msgServiceStatus(v.MngtStatus),
		WashServerID: s.serverID,
	}
}

func msgServiceStatus(v app.ServiceStatus) mngt_entity.ServiceStatus {
	status := mngt_entity.ServiceStatus{
		Available:        v.Available,
		DisabledOnServer: v.DisabledOnServer,
		IsConnected:      v.IsConnected,
		LastErr:          v.LastErr,
		UnpaidStations:   v.UnpaidStations,
		DateLastErr:      v.DateLastErr,
		ReconnectCount:   v.ReconnectCount,
	}
	return status
}

func msgStationStatus(v app.StationStatus) mngt_entity.StationStatus {
	return mngt_entity.StationStatus{
		ID:             int(v.ID),
		Info:           v.Info,
		Name:           v.Name,
		Status:         msgStatus(v.Status),
		CurrentBalance: v.CurrentBalance,
		CurrentProgram: v.CurrentProgram,
		ProgramName:    v.ProgramName,
		IP:             v.IP,
	}
}

func msgStatus(v app.Status) mngt_entity.Status {
	switch v {
	case app.StatusOffline:
		return mngt_entity.StatusOffline
	case app.StatusOnline:
		return mngt_entity.StatusOnline
	default:
		panic(fmt.Sprintf("unknown status %v", v))
	}
}
