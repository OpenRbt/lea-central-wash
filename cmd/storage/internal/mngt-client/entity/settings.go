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

type OpenwashingLog struct {
	ID        int64     `json:"id"`
	StationID int       `json:"stationId"`
	Text      string    `json:"text"`
	Type      *string   `json:"type,omitempty"`
	Level     string    `json:"level"`
	CreatedAt time.Time `json:"createdAt"`
}

func OpenwashingLogToRabbit(log app.OpenwashingLog) OpenwashingLog {
	return OpenwashingLog{
		ID:        log.ID,
		StationID: int(log.StationID),
		Text:      log.Text,
		Type:      log.Type,
		Level:     string(log.Level),
		CreatedAt: log.CreatedAt,
	}
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

func UpsertAdvertisingCampaignToApp(campaign AdvertisingCampaign) app.ManagementAdvertisingCampaign {
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

type ConfigString struct {
	Name        string `json:"name"`
	Value       string `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Deleted     bool   `json:"deleted"`
	Version     int    `json:"version"`
}

type ConfigInt struct {
	Name        string `json:"name"`
	Value       int64  `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Version     int    `json:"version"`
}

type ConfigBool struct {
	Name        string `json:"name"`
	Value       bool   `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Version     int    `json:"version"`
}

type StationConfigString struct {
	Name        string `json:"name"`
	StationID   int    `json:"stationId"`
	Value       string `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Version     int    `json:"version"`
}

type StationConfigInt struct {
	Name        string `json:"name"`
	StationID   int    `json:"stationId"`
	Value       int64  `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Version     int    `json:"version"`
}

type StationConfigBool struct {
	Name        string `json:"name"`
	StationID   int    `json:"stationId"`
	Value       bool   `json:"value"`
	Description string `json:"description"`
	Note        string `json:"note"`
	Version     int    `json:"version"`
}

func ConfigStringToRabbit(config app.ConfigString) ConfigString {
	return ConfigString{
		Name:        config.Name,
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Deleted:     config.Deleted,
		Version:     config.Version,
	}
}

func ConfigIntToRabbit(config app.ConfigInt) ConfigInt {
	return ConfigInt{
		Name:        config.Name,
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Version:     config.Version,
	}
}

func ConfigBoolToRabbit(config app.ConfigBool) ConfigBool {
	return ConfigBool{
		Name:        config.Name,
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Version:     config.Version,
	}
}

func StationConfigStringToRabbit(config app.StationConfigVar[string]) StationConfigString {
	return StationConfigString{
		Name:        config.Name,
		StationID:   int(config.StationID),
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Version:     config.Version,
	}
}

func StationConfigIntToRabbit(config app.StationConfigVar[int64]) StationConfigInt {
	return StationConfigInt{
		Name:        config.Name,
		StationID:   int(config.StationID),
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Version:     config.Version,
	}
}

func StationConfigBoolToRabbit(config app.StationConfigVar[bool]) StationConfigBool {
	return StationConfigBool{
		Name:        config.Name,
		StationID:   int(config.StationID),
		Value:       config.Value,
		Description: config.Description,
		Note:        config.Note,
		Version:     config.Version,
	}
}
