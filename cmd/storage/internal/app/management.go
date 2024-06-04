package app

import (
	"context"
	"time"
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

func (app *app) sendManagementSyncSignal() {
	if !app.IsManagementInit() {
		return
	}

	select {
	case app.mngtSvc.syncChannel <- struct{}{}:
	default:
	}
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

func (app *app) NotSendedOpenwashingLogs(ctx context.Context) ([]OpenwashingLog, error) {
	return app.repo.NotSendedOpenwashingLogs(ctx)
}

func (app *app) MarkOpenwashingLogSended(ctx context.Context, id int64) error {
	return app.repo.MarkOpenwashingLogSended(ctx, id)
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

func (app *app) DeleteAdvertisingCampaignFromManagement(ctx context.Context, id int64) error {
	_, err := app.repo.GetAdvertisingCampaignByID(ctx, id)
	if err != nil {
		return err
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
