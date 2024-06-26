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

type ProgramFilter struct {
	ID *int64 `json:"id"`
	Pagination
}

func ProgramFilterToApp(filter ProgramFilter) app.ProgramFilter {
	return app.ProgramFilter{
		ID:         filter.ID,
		Pagination: PaginationToApp(filter.Pagination),
	}
}

func ProgramToApp(program Program) app.Program {
	res := app.Program{
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

type AdvertisingCampaignFilter struct {
	StartDate *time.Time `json:"startDate"`
	EndDate   *time.Time `json:"endDate"`
	Pagination
}

func AdvertisingCampaignFilterToApp(filter AdvertisingCampaignFilter) app.AdvertisingCampaignFilter {
	return app.AdvertisingCampaignFilter{
		StartDate:  filter.StartDate,
		EndDate:    filter.EndDate,
		Pagination: app.Pagination(filter.Pagination),
	}
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

type User struct {
	ID         int    `json:"id"`
	Login      string `json:"login"`
	Password   string `json:"password"`
	FirstName  string `json:"firstName"`
	MiddleName string `json:"middleName"`
	LastName   string `json:"lastName"`
	IsAdmin    bool   `json:"isAdmin"`
	IsEngineer bool   `json:"isEngineer"`
	IsOperator bool   `json:"isOperator"`
	Deleted    bool   `json:"deleted"`
	Version    int    `json:"version"`
}

type UserCreation struct {
	Login      string  `json:"login"`
	Password   string  `json:"password"`
	FirstName  *string `json:"firstName,omitempty"`
	MiddleName *string `json:"middleName,omitempty"`
	LastName   *string `json:"lastName,omitempty"`
	IsAdmin    *bool   `json:"isAdmin,omitempty"`
	IsEngineer *bool   `json:"isEngineer,omitempty"`
	IsOperator *bool   `json:"isOperator,omitempty"`
}

type UserUpdate struct {
	Login      string  `json:"login"`
	FirstName  *string `json:"firstName,omitempty"`
	MiddleName *string `json:"middleName,omitempty"`
	LastName   *string `json:"lastName,omitempty"`
	IsAdmin    *bool   `json:"isAdmin,omitempty"`
	IsEngineer *bool   `json:"isEngineer,omitempty"`
	IsOperator *bool   `json:"isOperator,omitempty"`
}

type ChangePassword struct {
	Login       string `json:"login"`
	OldPassword string `json:"oldPassword"`
	NewPassword string `json:"newPassword"`
}

type UserFilter struct {
	Pagination
}

func UserToRabbit(user app.User) User {
	return User{
		ID:         user.ID,
		Login:      user.Login,
		Password:   user.Password,
		FirstName:  user.FirstName,
		MiddleName: user.MiddleName,
		LastName:   user.LastName,
		IsAdmin:    user.IsAdmin,
		IsEngineer: user.IsEngineer,
		IsOperator: user.IsOperator,
		Deleted:    user.Deleted,
		Version:    user.Version,
	}
}

func UserPageToRabbit(page app.Page[app.User]) Page[User] {
	return Page[User]{
		Items:      UsersToRabbit(page.Items),
		Page:       page.Page,
		PageSize:   page.PageSize,
		TotalPages: page.TotalPages,
		TotalCount: page.TotalCount,
	}
}

func UsersToRabbit(users []app.User) []User {
	l := []User{}
	for _, v := range users {
		l = append(l, UserToRabbit(v))
	}
	return l
}

func UserCreationToApp(user UserCreation) app.UserCreation {
	return app.UserCreation{
		Login:      user.Login,
		Password:   user.Password,
		FirstName:  user.FirstName,
		MiddleName: user.MiddleName,
		LastName:   user.LastName,
		IsAdmin:    user.IsAdmin,
		IsEngineer: user.IsEngineer,
		IsOperator: user.IsOperator,
	}
}

func UserUpdateToApp(user UserUpdate) app.UserUpdate {
	return app.UserUpdate{
		FirstName:  user.FirstName,
		MiddleName: user.MiddleName,
		LastName:   user.LastName,
		IsAdmin:    user.IsAdmin,
		IsEngineer: user.IsEngineer,
		IsOperator: user.IsOperator,
	}
}

func ChangePasswordToApp(password ChangePassword) app.UpdatePasswordData {
	return app.UpdatePasswordData{
		OldPassword: password.OldPassword,
		NewPassword: password.NewPassword,
	}
}

func UserFilterToApp(filter UserFilter) app.UserFilter {
	return app.UserFilter{
		Pagination: PaginationToApp(filter.Pagination),
	}
}

type AddServiceAmount struct {
	StationID int `json:"stationId"`
	Amount    int `json:"amount"`
}
