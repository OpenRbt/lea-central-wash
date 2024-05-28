package mngt_entity

import (
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
)

type ArgID[T any] struct {
	ID T `json:"id"`
}

type Program struct {
	ID                         int64   `json:"id"`
	Price                      int     `json:"price"`
	Name                       string  `json:"name"`
	PreflightEnabled           bool    `json:"preflightEnabled"`
	MotorSpeedPercent          int64   `json:"motorSpeedPercent"`
	PreflightMotorSpeedPercent int64   `json:"preflightMotorSpeedPercent"`
	Relays                     []Relay `json:"relays"`
	PreflightRelays            []Relay `json:"preflightRelays"`
	IsFinishingProgram         bool    `json:"isFinishingProgram"`
	Version                    int     `json:"version"`
	Force                      bool    `json:"force"`
}

type Relay struct {
	ID      int `json:"id"`
	TimeOn  int `json:"timeOn"`
	TimeOff int `json:"timeOff"`
}

func ProgramToApp(program Program) app.ManagementProgram {
	res := app.ManagementProgram{
		ID:                         program.ID,
		Price:                      program.Price,
		Name:                       program.Name,
		PreflightEnabled:           program.PreflightEnabled,
		MotorSpeedPercent:          program.MotorSpeedPercent,
		PreflightMotorSpeedPercent: program.PreflightMotorSpeedPercent,
		IsFinishingProgram:         program.IsFinishingProgram,
		Version:                    program.Version,
		Force:                      program.Force,
	}

	for _, relay := range program.Relays {
		res.Relays = append(res.Relays, app.Relay{
			ID:      relay.ID,
			TimeOn:  relay.TimeOn,
			TimeOff: relay.TimeOff,
		})
	}

	for _, relay := range program.PreflightRelays {
		res.PreflightRelays = append(res.PreflightRelays, app.Relay{
			ID:      relay.ID,
			TimeOn:  relay.TimeOn,
			TimeOff: relay.TimeOff,
		})
	}

	return res
}

func ProgramToRabbit(program app.Program) Program {
	res := Program{
		ID:                         program.ID,
		Price:                      program.Price,
		Name:                       program.Name,
		PreflightEnabled:           program.PreflightEnabled,
		MotorSpeedPercent:          program.MotorSpeedPercent,
		PreflightMotorSpeedPercent: program.PreflightMotorSpeedPercent,
		IsFinishingProgram:         program.IsFinishingProgram,
		Version:                    program.Version,
	}

	for _, relay := range program.Relays {
		res.Relays = append(res.Relays, Relay{
			ID:      relay.ID,
			TimeOn:  relay.TimeOn,
			TimeOff: relay.TimeOff,
		})
	}

	for _, relay := range program.PreflightRelays {
		res.PreflightRelays = append(res.PreflightRelays, Relay{
			ID:      relay.ID,
			TimeOn:  relay.TimeOn,
			TimeOff: relay.TimeOff,
		})
	}

	return res
}

type DiscountProgram struct {
	Discount  int64 `json:"discount"`
	ProgramID int64 `json:"programId"`
}

type AdvertisingCampaign struct {
	ID               int64             `json:"id"`
	Name             string            `json:"name"`
	DefaultDiscount  int64             `json:"defaultDiscount"`
	DiscountPrograms []DiscountProgram `json:"discountPrograms"`
	StartDate        time.Time         `json:"startDate"`
	EndDate          time.Time         `json:"endDate"`
	StartMinute      int64             `json:"startMinute"`
	EndMinute        int64             `json:"endMinute"`
	Weekday          []string          `json:"weekday"`
	Enabled          bool              `json:"enabled"`
	Version          int               `json:"version"`
	Deleted          bool              `json:"deleted"`
	Force            bool              `json:"force"`
}

func AdvertisingCampaignToApp(campaign AdvertisingCampaign) app.AdvertisingCampaign {
	res := app.AdvertisingCampaign{
		ID:              campaign.ID,
		Name:            campaign.Name,
		DefaultDiscount: campaign.DefaultDiscount,
		StartDate:       campaign.StartDate,
		EndDate:         campaign.EndDate,
		StartMinute:     campaign.StartMinute,
		EndMinute:       campaign.EndMinute,
		Weekday:         campaign.Weekday,
		Enabled:         campaign.Enabled,
		Version:         campaign.Version,
	}

	for _, program := range campaign.DiscountPrograms {
		res.DiscountPrograms = append(res.DiscountPrograms, app.DiscountProgram{
			Discount:  program.Discount,
			ProgramID: program.ProgramID,
		})
	}

	return res
}

func UpdateAdvertisingCampaignToApp(campaign AdvertisingCampaign) app.ManagementAdvertisingCampaign {
	res := app.ManagementAdvertisingCampaign{
		ID:              campaign.ID,
		Name:            campaign.Name,
		DefaultDiscount: campaign.DefaultDiscount,
		StartDate:       campaign.StartDate,
		EndDate:         campaign.EndDate,
		StartMinute:     campaign.StartMinute,
		EndMinute:       campaign.EndMinute,
		Weekday:         campaign.Weekday,
		Enabled:         campaign.Enabled,
		Version:         campaign.Version,
		Force:           campaign.Force,
	}

	for _, program := range campaign.DiscountPrograms {
		res.DiscountPrograms = append(res.DiscountPrograms, app.DiscountProgram{
			Discount:  program.Discount,
			ProgramID: program.ProgramID,
		})
	}

	return res
}

func AdvertisingCampaignToRabbit(campaign app.AdvertisingCampaign) AdvertisingCampaign {
	res := AdvertisingCampaign{
		ID:              campaign.ID,
		Name:            campaign.Name,
		DefaultDiscount: campaign.DefaultDiscount,
		StartDate:       campaign.StartDate,
		EndDate:         campaign.EndDate,
		StartMinute:     campaign.StartMinute,
		EndMinute:       campaign.EndMinute,
		Weekday:         campaign.Weekday,
		Enabled:         campaign.Enabled,
		Version:         campaign.Version,
		Deleted:         campaign.Deleted,
	}

	for _, program := range campaign.DiscountPrograms {
		res.DiscountPrograms = append(res.DiscountPrograms, DiscountProgram{
			Discount:  program.Discount,
			ProgramID: program.ProgramID,
		})
	}

	return res
}