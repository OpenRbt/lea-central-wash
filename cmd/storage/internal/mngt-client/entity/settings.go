package mngt_entity

import (
	"fmt"
	"strconv"
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

type Task struct {
	ID         int        `json:"id"`
	StationID  int        `json:"stationId"`
	VersionID  *int       `json:"versionId,omitempty"`
	Type       string     `json:"type"`
	Status     string     `json:"status"`
	RetryCount int        `json:"retryCount"`
	Error      *string    `json:"error,omitempty"`
	CreatedAt  time.Time  `json:"createdAt"`
	StartedAt  *time.Time `json:"startedAt,omitempty"`
	StoppedAt  *time.Time `json:"stoppedAt,omitempty"`
	Version    int        `json:"version"`
}

type CreateTask struct {
	StationID int    `json:"stationId"`
	VersionID *int   `json:"versionId,omitempty"`
	Type      string `json:"type"`
}

type TaskFilter struct {
	Pagination
	StationsID []int    `json:"stationsId,omitempty"`
	Statuses   []string `json:"statuses,omitempty"`
	Types      []string `json:"types,omitempty"`
	Sort       *string  `json:"sort,omitempty"`
}

type CopyBufferedFirmware struct {
	StationID       int `json:"stationId"`
	CopyToStationID int `json:"copyToStationId"`
}

func CreateTaskToApp(task CreateTask) (app.CreateTask, error) {
	taskType, err := TaskTypeToApp(task.Type)
	if err != nil {
		return app.CreateTask{}, err
	}
	return app.CreateTask{
		StationID: app.StationID(task.StationID),
		VersionID: task.VersionID,
		Type:      taskType,
	}, nil
}

func TaskFilterToApp(flter TaskFilter) (app.TaskFilter, error) {
	var taskTypes []app.TaskType = nil
	for _, v := range flter.Types {
		t, err := TaskTypeToApp(v)
		if err != nil {
			return app.TaskFilter{}, err
		}
		taskTypes = append(taskTypes, t)
	}

	var taskStatuses []app.TaskStatus = nil
	for _, v := range flter.Statuses {
		s, err := TaskStatusToApp(v)
		if err != nil {
			return app.TaskFilter{}, err
		}
		taskStatuses = append(taskStatuses, s)
	}

	var stationsID []app.StationID = nil
	for _, v := range flter.StationsID {
		stationsID = append(stationsID, app.StationID(v))
	}

	var sort *app.TaskSort = nil
	if flter.Sort != nil {
		s, err := TaskSortToApp(*flter.Sort)
		if err != nil {
			return app.TaskFilter{}, err
		}
		sort = &s
	}

	return app.TaskFilter{
		Pagination: PaginationToApp(flter.Pagination),
		Types:      taskTypes,
		StationsID: stationsID,
		Statuses:   taskStatuses,
		Sort:       sort,
	}, nil
}

func TaskToRabbit(task app.Task) Task {
	return Task{
		ID:         task.ID,
		StationID:  int(task.StationID),
		VersionID:  task.VersionID,
		Type:       string(task.Type),
		Status:     string(task.Status),
		RetryCount: task.RetryCount,
		Error:      task.Error,
		CreatedAt:  task.CreatedAt,
		StartedAt:  task.StartedAt,
		StoppedAt:  task.StoppedAt,
		Version:    task.Version,
	}
}

func TaskPageToRabbit(page app.Page[app.Task]) Page[Task] {
	return Page[Task]{
		Items:      TasksToRabbit(page.Items),
		Page:       page.Page,
		PageSize:   page.PageSize,
		TotalPages: page.TotalPages,
		TotalCount: page.TotalCount,
	}
}

func TasksToRabbit(tasks []app.Task) []Task {
	l := []Task{}
	for _, v := range tasks {
		l = append(l, TaskToRabbit(v))
	}
	return l
}

func TaskTypeToApp(taskType string) (app.TaskType, error) {
	switch taskType {
	case string(app.BuildTaskType):
		return app.BuildTaskType, nil
	case string(app.GetVersionsTaskType):
		return app.GetVersionsTaskType, nil
	case string(app.PullFirmwareTaskType):
		return app.PullFirmwareTaskType, nil
	case string(app.RebootTaskType):
		return app.RebootTaskType, nil
	case string(app.SetVersionTaskType):
		return app.SetVersionTaskType, nil
	case string(app.UpdateTaskType):
		return app.UpdateTaskType, nil
	default:
		return app.TaskType(""), fmt.Errorf("%w: unknown task type: %s", app.ErrWrongParameter, taskType)
	}
}

func TaskStatusToApp(status string) (app.TaskStatus, error) {
	switch status {
	case string(app.QueueTaskStatus):
		return app.QueueTaskStatus, nil
	case string(app.StartedTaskStatus):
		return app.StartedTaskStatus, nil
	case string(app.CompletedTaskStatus):
		return app.CompletedTaskStatus, nil
	case string(app.ErrorTaskStatus):
		return app.ErrorTaskStatus, nil
	case string(app.CanceledTaskStatus):
		return app.CanceledTaskStatus, nil
	default:
		return app.TaskStatus(""), fmt.Errorf("%w: unknown task status: %s", app.ErrWrongParameter, status)
	}
}

func TaskSortToApp(sort string) (app.TaskSort, error) {
	switch sort {
	case string(app.CreatedAtAscTaskSort):
		return app.CreatedAtAscTaskSort, nil
	case string(app.CreatedAtDescTaskSort):
		return app.CreatedAtDescTaskSort, nil
	default:
		return app.TaskSort(""), fmt.Errorf("%w: unknown task sort: %s", app.ErrWrongParameter, sort)
	}
}

type FirmwareVersion struct {
	ID         int       `json:"id"`
	StationID  int       `json:"stationId"`
	IsCurrent  bool      `json:"isCurrent"`
	HashLua    string    `json:"hashLua"`
	HashEnv    string    `json:"hashEnv"`
	HashBinar  string    `json:"hashBinar"`
	BuiltAt    time.Time `json:"builtAt"`
	CommitedAt time.Time `json:"commitedAt"`
}

func FirmwareVersionToRabbit(version app.FirmwareVersion, stationID int) FirmwareVersion {
	return FirmwareVersion{
		ID:         version.ID,
		StationID:  stationID,
		IsCurrent:  version.IsCurrent,
		HashLua:    version.HashLua,
		HashEnv:    version.HashEnv,
		HashBinar:  version.HashBinar,
		BuiltAt:    version.BuiltAt,
		CommitedAt: version.CommitedAt,
	}
}

func FirmwareVersionsToRabbit(versions []app.FirmwareVersion, stationID int) []FirmwareVersion {
	l := []FirmwareVersion{}
	for _, v := range versions {
		l = append(l, FirmwareVersionToRabbit(v, stationID))
	}
	return l
}

type Button struct {
	ID        int `json:"id"`
	ProgramID int `json:"programId"`
}

type StationUpdate struct {
	ID           int         `json:"id"`
	Name         *string     `json:"name,omitempty"`
	PreflightSec *int        `json:"preflightSec,omitempty"`
	RelayBoard   *string     `json:"relayBoard,omitempty"`
	Buttons      []Button    `json:"buttons,omitempty"`
	CardReader   *CardReader `json:"cardReader,omitempty"`
}

type Station struct {
	ID           int        `json:"id"`
	Name         string     `json:"name"`
	PreflightSec int        `json:"preflightSec"`
	RelayBoard   string     `json:"relayBoard"`
	Buttons      []Button   `json:"buttons"`
	Version      int        `json:"version"`
	Deleted      bool       `json:"deleted"`
	CardReader   CardReader `json:"cardReader"`
}

type CardReader struct {
	Type string  `json:"type"`
	Host *string `json:"host,omitempty"`
	Port *int    `json:"port,omitempty"`
}

func StationUpdateToApp(station StationUpdate) (app.StationUpdate, error) {
	var cardReader *app.CardReaderConfig = nil
	if station.CardReader != nil {
		cr, err := CardReaderToApp(*station.CardReader, station.ID)
		if err != nil {
			return app.StationUpdate{}, err
		}
		cardReader = app.Ptr(cr)
	}
	return app.StationUpdate{
		Name:         station.Name,
		PreflightSec: station.PreflightSec,
		RelayBoard:   station.RelayBoard,
		Buttons:      ButtonsToApp(station.Buttons),
		CardReader:   cardReader,
	}, nil
}

func StationToRabbit(station app.StationConfig) (Station, error) {
	cr, err := CardReaderToRabbit(station.CardReader)
	if err != nil {
		return Station{}, nil
	}

	return Station{
		ID:           int(station.ID),
		Name:         station.Name,
		PreflightSec: station.PreflightSec,
		RelayBoard:   station.RelayBoard,
		Buttons:      ButtonsToRabbit(station.Programs),
		Version:      station.Version,
		Deleted:      station.Deleted,
		CardReader:   cr,
	}, nil
}

func CardReaderToRabbit(cardReader app.CardReaderConfig) (CardReader, error) {
	var port *int
	if cardReader.Port != "" {
		p, err := strconv.Atoi(cardReader.Port)
		if err != nil {
			return CardReader{}, err
		}
		port = &p
	}
	var host *string
	if cardReader.Host != "" {
		host = &cardReader.Host
	}

	t, err := rabbitCardReaderType(cardReader.CardReaderType)
	if err != nil {
		return CardReader{}, err
	}

	return CardReader{
		Type: t,
		Host: host,
		Port: port,
	}, nil
}

func CardReaderToApp(cardReader CardReader, stationID int) (app.CardReaderConfig, error) {
	var host string
	var port string
	if cardReader.Host != nil {
		host = *cardReader.Host
	}
	if cardReader.Port != nil {
		port = fmt.Sprintf("%d", *cardReader.Port)
	}
	t, err := appCardReaderType(cardReader.Type)
	if err != nil {
		return app.CardReaderConfig{}, err
	}

	return app.CardReaderConfig{
		StationID:      app.StationID(stationID),
		CardReaderType: t,
		Host:           host,
		Port:           port,
	}, err
}

func ButtonToRabbit(programs app.Program) Button {
	return Button{
		ID:        programs.ButtonID,
		ProgramID: int(programs.ID),
	}
}

func ButtonToApp(button Button) app.StationProgram {
	return app.StationProgram{
		ButtonID:  button.ID,
		ProgramID: button.ProgramID,
	}
}

func ButtonsToRabbit(programs []app.Program) []Button {
	buttons := []Button{}
	for _, v := range programs {
		buttons = append(buttons, ButtonToRabbit(v))
	}
	return buttons
}

func ButtonsToApp(programs []Button) []app.StationProgram {
	buttons := []app.StationProgram{}
	for _, v := range programs {
		buttons = append(buttons, ButtonToApp(v))
	}
	return buttons
}

func appCardReaderType(cardReader string) (string, error) {
	switch cardReader {
	case "paymentWorld":
		return "PAYMENT_WORLD", nil
	case "notUsed":
		return "NOT_USED", nil
	case "vendotek":
		return "VENDOTEK", nil
	default:
		return "", fmt.Errorf("%w: unknown card reader: %s", app.ErrWrongParameter, cardReader)
	}
}

func rabbitCardReaderType(cardReader string) (string, error) {
	switch cardReader {
	case "PAYMENT_WORLD":
		return "paymentWorld", nil
	case "NOT_USED":
		return "notUsed", nil
	case "VENDOTEK":
		return "vendotek", nil
	default:
		return "", fmt.Errorf("%w: unknown card reader: %s", app.ErrWrongParameter, cardReader)
	}
}
