package app

import (
	"context"
	"errors"
	"time"

	"golang.org/x/crypto/bcrypt"
)

type (
	ManagementRabbitConfig struct {
		ServerID       string
		ServerPassword string
	}

	ManagementProgram struct {
		ID                         int64
		Price                      int
		Name                       string
		PreflightEnabled           bool
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		Relays                     []Relay
		PreflightRelays            []Relay
		IsFinishingProgram         bool
		Version                    int
		Force                      bool
	}

	ManagementAdvertisingCampaign struct {
		ID               int64
		DefaultDiscount  int64
		DiscountPrograms []DiscountProgram
		EndDate          time.Time
		EndMinute        int64
		StartDate        time.Time
		StartMinute      int64
		Weekday          []string
		Enabled          bool
		Name             string
		Version          int
		Force            bool
	}
)

func (app *app) SendManagementSyncSignal() {
	if !app.IsManagementInit() {
		return
	}

	select {
	case app.mngtSvc.syncChannel <- struct{}{}:
	default:
	}
}

func (a *app) AddServiceAmountForManagement(ctx context.Context, id StationID, money int) error {
	data, err := a.Get(id)
	if err != nil {
		return err
	}

	data.ServiceMoney += money

	err = a.Set(data)
	if err != nil {
		return err
	}
	return nil
}

func (app *app) StationUpdateForManagement(ctx context.Context, id StationID, station StationUpdate) (StationConfig, error) {
	_, err := app.repo.StationConfig(id)
	if err != nil {
		return StationConfig{}, err
	}

	stationConfig, err := app.repo.StationUpdate(ctx, id, station)
	if err != nil {
		return StationConfig{}, err
	}

	err = app.updateConfig("SetStationProgramForManagement")
	if err != nil {
		return StationConfig{}, err
	}

	return stationConfig, nil
}

func (app *app) StationGetForManagement(ctx context.Context, id StationID) (StationConfig, error) {
	return app.StationConfig(id)
}

func (app *app) GetTasksForManagement(ctx context.Context, filter TaskFilter) (Page[Task], error) {
	return app.GetListTasks(filter)
}

func (app *app) GetTaskByIdForManagement(ctx context.Context, id int) (Task, error) {
	return app.GetTask(id)
}

func (app *app) CreateTaskForManagement(ctx context.Context, createTask CreateTask) (Task, error) {
	return app.CreateTask(createTask)
}

func (app *app) CopyFirmwareForManagement(ctx context.Context, stationID StationID, copyToID StationID) error {
	return app.CopyFirmware(stationID, copyToID)
}

func (app *app) GetVersionBufferedForManagement(ctx context.Context, stationID StationID) (FirmwareVersion, error) {
	return app.GetVersionBuffered(stationID)
}

func (app *app) GetVersionsForManagement(ctx context.Context, stationID StationID) ([]FirmwareVersion, error) {
	return app.GetVersions(stationID)
}

func (app *app) GetUsersForManagement(ctx context.Context, filter UserFilter) (Page[User], error) {
	users, total, err := app.repo.Users(ctx, filter)
	if err != nil {
		return Page[User]{}, err
	}

	return NewPage(users, filter.Pagination, total), nil
}

func (app *app) GetUserForManagement(ctx context.Context, login string) (User, error) {
	user, err := app.repo.User(login)
	if err != nil {
		return User{}, err
	}

	return user, nil
}

func (app *app) CreateUsersForManagement(ctx context.Context, userCreation UserCreation) (User, error) {
	if !app.isPasswordUnique(ctx, userCreation.Password) {
		return User{}, ErrPasswordNotUnique
	}

	_, err := app.repo.User(userCreation.Login)
	if err != nil && !errors.Is(err, ErrNotFound) {
		return User{}, err
	} else if err == nil {
		return User{}, ErrLoginNotUnique
	}

	userCreation, err = defaultUserSettings(userCreation)
	if err != nil {
		return User{}, err
	}

	return app.repo.CreateUser(userCreation)
}

func (app *app) UpdateUsersForManagement(ctx context.Context, login string, userUpdate UserUpdate) (User, error) {
	_, err := app.repo.User(login)
	if err != nil {
		return User{}, err
	}

	return app.repo.UpdateUser(login, userUpdate)
}

func (app *app) ChangeUserPasswordForManagement(ctx context.Context, login string, password UpdatePasswordData) (User, error) {
	if !app.isPasswordUnique(ctx, password.NewPassword) {
		return User{}, ErrPasswordNotUnique
	}

	user, err := app.repo.User(login)
	if err != nil {
		return User{}, err
	}

	errOldPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(password.OldPassword))
	if errOldPassword != nil {
		return User{}, ErrWrongPassword
	}

	newPassword, errNewPassword := bcrypt.GenerateFromPassword([]byte(password.NewPassword), bcrypt.DefaultCost)
	if errNewPassword != nil {
		return User{}, errNewPassword
	}

	newPasswordStr := string(newPassword)
	user, err = app.repo.UpdateUser(login, UserUpdate{NewPassword: &newPasswordStr})
	if err != nil {
		return User{}, err
	}

	return user, nil
}

func (app *app) DeleteUsersForManagement(ctx context.Context, login string) (User, error) {
	user, err := app.repo.User(login)
	if err != nil {
		return User{}, err
	}

	if user.IsAdmin {
		_, total, err := app.repo.Users(ctx, UserFilter{IsAdmin: Ptr(true)})
		if err != nil {
			return User{}, err
		}

		if total <= 1 {
			return User{}, ErrRemovingOnlyAdmin
		}
	}

	return app.repo.DeleteUser(ctx, login)
}

func (app *app) GetProgramsForManagement(ctx context.Context, filter ProgramFilter) (Page[Program], error) {
	programs, total, err := app.repo.GetPrograms(ctx, filter)
	if err != nil {
		return Page[Program]{}, err
	}

	return NewPage(programs, filter.Pagination, total), nil
}

func (app *app) SetProgramFromManagement(ctx context.Context, program Program) (Program, error) {
	return app.repo.SetProgram(ctx, program)
}

func (app *app) NotSendedPrograms(ctx context.Context) ([]Program, error) {
	return app.repo.NotSendedPrograms(ctx)
}

func (app *app) MarkProgramSended(ctx context.Context, id int64) error {
	return app.repo.MarkProgramSended(ctx, id)
}

func (app *app) GetAdvertisingCampaignByIDForManagement(ctx context.Context, id int64) (AdvertisingCampaign, error) {
	return app.repo.GetAdvertisingCampaignByID(ctx, id)
}

func (app *app) GetAdvertisingCampaignsForManagement(ctx context.Context, filter AdvertisingCampaignFilter) (Page[AdvertisingCampaign], error) {
	campaigns, total, err := app.repo.GetAdvertisingCampaigns(ctx, filter)
	if err != nil {
		return Page[AdvertisingCampaign]{}, err
	}

	return NewPage(campaigns, filter.Pagination, total), nil
}

func (app *app) AddAdvertisingCampaignFromManagement(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error) {
	return app.repo.AddAdvertisingCampaign(ctx, campaign)
}

func (app *app) EditAdvertisingCampaignFromManagement(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error) {
	_, err := app.repo.GetAdvertisingCampaignByID(ctx, campaign.ID)
	if err != nil {
		return AdvertisingCampaign{}, err
	}

	return app.repo.EditAdvertisingCampaign(ctx, campaign)
}

func (app *app) DeleteAdvertisingCampaignFromManagement(ctx context.Context, id int64) (AdvertisingCampaign, error) {
	_, err := app.repo.GetAdvertisingCampaignByID(ctx, id)
	if err != nil {
		return AdvertisingCampaign{}, err
	}

	return app.repo.DeleteAdvertisingCampaign(ctx, id)
}

func (app *app) UpsertAdvertisingCampaignFromManagement(ctx context.Context, advert ManagementAdvertisingCampaign) (AdvertisingCampaign, error) {
	return app.repo.UpsertAdvertisingCampaignFromManagement(ctx, advert)
}

func (app *app) NotSendedAdvertisingCampaigns(ctx context.Context) ([]AdvertisingCampaign, error) {
	return app.repo.NotSendedAdvertisingCampaigns(ctx)
}

func (app *app) MarkAdvertisingCampaignSended(ctx context.Context, id int64) error {
	return app.repo.MarkAdvertisingCampaignSended(ctx, id)
}
