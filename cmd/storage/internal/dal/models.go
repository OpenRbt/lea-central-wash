package dal

import (
	"encoding/json"
	"strings"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/lib/pq"
)

func appSetUsers(v []resUser) []app.UserData {
	var res []app.UserData
	for i := range v {
		res = append(res, app.UserData{
			ID:         v[i].ID,
			Login:      v[i].Login,
			Password:   v[i].Password,
			FirstName:  &v[i].FirstName,
			MiddleName: &v[i].MiddleName,
			LastName:   &v[i].LastName,
			IsAdmin:    &v[i].IsAdmin,
			IsOperator: &v[i].IsOperator,
			IsEngineer: &v[i].IsEngineer,
		})
	}
	return res
}

func appSetStation(v []resStation) []app.SetStation {
	var res []app.SetStation
	for i := range v {
		res = append(res, app.SetStation{
			ID:           v[i].ID,
			Name:         v[i].Name,
			PreflightSec: v[i].PreflightSec,
			RelayBoard:   v[i].RelayBoard,
			IsActive:     v[i].Hash != nil,
		})
	}
	return res
}

func appStation(v resStation) app.SetStation {
	return app.SetStation{
		ID:           v.ID,
		Name:         v.Name,
		PreflightSec: v.PreflightSec,
		RelayBoard:   v.RelayBoard,
	}
}

func appStationsVariables(v []resStationsVariables) []app.StationsVariables {
	var res []app.StationsVariables
	id := app.StationID(-1)
	count := -1
	for i := range v {
		if id != v[i].ID {
			res = append(res, app.StationsVariables{
				ID:      v[i].ID,
				Name:    v[i].Name,
				KeyPair: []app.KeyPair{},
			})
			id = v[i].ID
			count++
		}
		res[count].KeyPair = append(res[count].KeyPair, app.KeyPair{
			Key:   v[i].Key,
			Value: v[i].Value,
		})
	}
	return res
}

func appProgram(p resProgram) app.Program {
	return app.Program{
		ID:                         p.ID,
		Name:                       p.Name,
		Price:                      p.Price,
		PreflightEnabled:           p.PreflightEnabled,
		MotorSpeedPercent:          p.MotorSpeedPercent,
		PreflightMotorSpeedPercent: p.PreflightMotorSpeedPercent,
		IsFinishingProgram:         p.IsFinishingProgram,
		Relays:                     appProgramRelays(p.Relays),
		PreflightRelays:            appProgramRelays(p.PreflightRelays),
		Version:                    p.Version,
	}
}

func appPrograms(programs []resProgram) (res []app.Program) {
	for _, program := range programs {
		res = append(res, appProgram(program))
	}
	return res
}

func appProgramRelays(jsonRelays string) (res []app.Relay) {
	err := json.Unmarshal([]byte(jsonRelays), &res)

	if err != nil {
		panic(err)
	}
	return res
}

func dalProgramRelays(relays []app.Relay) (jsonRelays string) {
	bytes, err := json.Marshal(relays)

	if err != nil {
		panic(err)
	}

	jsonRelays = string(bytes)
	return jsonRelays
}

func appKasse(k resKasse) (res app.Kasse) {
	res.CashierFullName = k.CashierFullName
	res.CashierINN = k.CashierINN
	res.ReceiptItem = k.ReceiptItem
	res.TaxType = k.TaxType

	return res
}

func appStationProgram(p []resStationProgram) (res []app.StationProgram) {
	for i := range p {
		res = append(res, app.StationProgram{
			ButtonID:  p[i].ButtonID,
			ProgramID: p[i].ProgramID,
		})
	}
	return res
}

func appStationConfig(p []resStationConfig) (res app.StationConfig) {
	if len(p) == 0 {
		return
	}
	res.ID = p[0].ID
	res.Name = p[0].Name
	res.PreflightSec = p[0].PreflightSec
	res.RelayBoard = p[0].RelayBoard

	for i := range p {
		res.Programs = append(res.Programs, app.Program{
			ID:                         p[i].ProgramID,
			ButtonID:                   p[i].ButtonID,
			Name:                       p[i].ProgramName,
			Price:                      p[i].Price,
			PreflightEnabled:           p[i].PreflightEnabled,
			MotorSpeedPercent:          p[i].MotorSpeedPercent,
			PreflightMotorSpeedPercent: p[i].PreflightMotorSpeedPercent,
			IsFinishingProgram:         p[i].IsFinishingProgram,
			Relays:                     appProgramRelays(p[i].Relays),
			PreflightRelays:            appProgramRelays(p[i].PreflightRelays),
		})
	}
	return res
}

func appCollectionReportsByDate(r []resCollectionReportByDate) (res []app.CollectionReportWithUser) {
	for i := range r {
		res = append(res, app.CollectionReportWithUser{
			StationID:    r[i].StationID,
			Banknotes:    r[i].Banknotes,
			CarsTotal:    r[i].CarsTotal,
			Coins:        r[i].Coins,
			Electronical: r[i].Electronical,
			Service:      r[i].Service,
			Bonuses:      r[i].Bonuses,
			Ctime:        r[i].Ctime,
			User:         r[i].User,
			QrMoney:      r[i].QrMoney,
		})
	}
	return res
}

func appStationsStat(res []resRelayReport, relay []resRelayStats) app.StationsStat {
	report := app.StationsStat{}
	for i := range res {
		tmp, ok := report[res[i].StationID]
		if !ok {
			tmp = app.StationStat{
				StationID:    res[i].StationID,
				PumpTimeOn:   0,
				RelayStats:   []app.RelayStat{},
				ProgramStats: []app.ProgramStat{},
			}
		}
		tmp.PumpTimeOn = tmp.PumpTimeOn + res[i].PumpTimeOn
		tmp.ProgramStats = append(tmp.ProgramStats, app.ProgramStat{
			ProgramID:   res[i].ProgramID,
			ProgramName: res[i].ProgramName,
			TimeOn:      res[i].TimeOn,
		})
		report[res[i].StationID] = tmp
	}
	for i := range relay {
		tmp, ok := report[relay[i].StationID]
		if ok {
			tmp.RelayStats = append(tmp.RelayStats, app.RelayStat{
				RelayID:       relay[i].RelayID,
				SwitchedCount: relay[i].SwitchedCount,
				TotalTimeOn:   relay[i].TotalTimeOn,
			})
		}
		report[relay[i].StationID] = tmp
	}

	return report
}

func dalDiscountPrograms(programs []app.DiscountProgram) string {
	bytes, err := json.Marshal(programs)
	if err != nil {
		panic(err)
	}

	return string(bytes)
}

func dalAdvertisingCampaign(a app.AdvertisingCampaign) argAdvertisingCampaign {
	return argAdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: dalDiscountPrograms(a.DiscountPrograms),
		EndDate:          a.EndDate,
		EndMinute:        a.EndMinute,
		ID:               a.ID,
		StartDate:        a.StartDate,
		StartMinute:      a.StartMinute,
		Weekday:          strings.Join(a.Weekday, ","),
		Enabled:          a.Enabled,
		Name:             a.Name,
	}
}

func appDiscountPrograms(programs string) []app.DiscountProgram {
	var discountPrograms []app.DiscountProgram
	err := json.Unmarshal([]byte(programs), &discountPrograms)
	if err != nil {
		panic(err)
	}

	return discountPrograms
}

func appAdvertisingCampaign(a resAdvertisingCampaign) *app.AdvertisingCampaign {
	weekday := []string{}
	if a.Weekday != "" {
		weekday = strings.Split(a.Weekday, ",")
	}
	return &app.AdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: appDiscountPrograms(a.DiscountPrograms),
		EndDate:          a.EndDate,
		EndMinute:        a.EndMinute,
		ID:               a.ID,
		StartDate:        a.StartDate,
		StartMinute:      a.StartMinute,
		Weekday:          weekday,
		Enabled:          a.Enabled,
		Name:             a.Name,
		Version:          a.Version,
		Deleted:          a.Deleted,
	}
}

func appAdvertisingCampaigns(a []resAdvertisingCampaign) []app.AdvertisingCampaign {
	res := []app.AdvertisingCampaign{}
	for i := range a {
		res = append(res, *appAdvertisingCampaign(a[i]))
	}
	return res
}

func appConfigInt(a resGetConfigInt) app.ConfigInt {
	return app.ConfigInt{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		Version:     a.Version,
	}
}

func appConfigBool(a resGetConfigBool) app.ConfigBool {
	return app.ConfigBool{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		Version:     a.Version,
	}
}

func appConfigString(a resGetConfigString) app.ConfigString {
	return app.ConfigString{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		Deleted:     a.Deleted,
		Version:     a.Version,
	}
}

func appConfigStrings(a []resGetConfigString) []app.ConfigString {
	l := []app.ConfigString{}
	for _, v := range a {
		l = append(l, appConfigString(v))
	}
	return l
}

func appConfigInts(a []resGetConfigInt) []app.ConfigInt {
	l := []app.ConfigInt{}
	for _, v := range a {
		l = append(l, appConfigInt(v))
	}
	return l
}

func appConfigBools(a []resGetConfigBool) []app.ConfigBool {
	l := []app.ConfigBool{}
	for _, v := range a {
		l = append(l, appConfigBool(v))
	}
	return l
}

func appStationConfigVar[T comparable](a resGetStationConfigVar[T]) app.StationConfigVar[T] {
	return app.StationConfigVar[T]{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   a.StationID,
		Version:     a.Version,
	}
}

func dalStationConfigVar[T comparable](a app.StationConfigVar[T]) argSetStationConfigVar[T] {
	return argSetStationConfigVar[T]{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   a.StationID,
	}
}

func appConfigVars[T comparable](a []resGetStationConfigVar[T]) []app.StationConfigVar[T] {
	l := []app.StationConfigVar[T]{}
	for _, v := range a {
		l = append(l, appStationConfigVar(v))
	}
	return l
}

func appRabbitMessages(a []resRabbitMessage) []app.RabbitMessage {
	res := []app.RabbitMessage{}
	for _, msg := range a {
		res = append(res, appRabbitMessage(msg))
	}
	return res
}

func appRabbitMessage(a resRabbitMessage) app.RabbitMessage {
	return app.RabbitMessage{
		ID:          app.RabbitMessageID(a.ID),
		MessageType: a.MessageType,
		Payload:     a.Payload,
		CreatedAt:   a.CreatedAt,
		IsSent:      a.Sent,
		SentAt:      a.SentAt,
	}
}
func appRabbitMoneyReports(r []resRabbitMoneyReport) []app.RabbitMoneyReport {
	res := []app.RabbitMoneyReport{}
	for _, rabbitReport := range r {
		res = append(res, appRabbitMoneyReport(rabbitReport))
	}
	return res
}

func appRabbitMoneyReport(r resRabbitMoneyReport) app.RabbitMoneyReport {
	return app.RabbitMoneyReport{
		ID:          app.RabbitMessageID(r.ID),
		MessageType: r.MessageType,
		MoneyReport: app.MoneyReport{
			StationID:    app.StationID(r.StationID),
			Banknotes:    r.Banknotes,
			CarsTotal:    r.CarsTotal,
			Coins:        r.Coins,
			Electronical: r.Electronical,
			Service:      r.Service,
			Bonuses:      r.Bonuses,
			SessionID:    r.SessionID,
			QrMoney:      r.QrMoney,
		},
		CreatedAt:   r.CreatedAt,
		IsSent:      r.Sent,
		SentAt:      r.SentAt,
		MessageUUID: r.MessageUUID.UUID,
	}
}

func appListBuildScripts(buildScripts []resBuildScript) ([]app.BuildScript, error) {
	var appBuildScripts []app.BuildScript

	for i := 0; i < len(buildScripts); i++ {
		bs, err := appBuildScript(buildScripts[i])
		if err != nil {
			return nil, err
		}

		appBuildScripts = append(appBuildScripts, bs)
	}

	return appBuildScripts, nil
}

func appBuildScript(buildScript resBuildScript) (app.BuildScript, error) {
	var commands []string
	err := json.Unmarshal([]byte(buildScript.Commands), &commands)
	if err != nil {
		return app.BuildScript{}, err
	}

	return app.BuildScript{
		ID:        buildScript.ID,
		Name:      buildScript.Name,
		StationID: app.StationID(buildScript.StationID),
		Commands:  commands,
	}, nil
}

func appOpenwashingLog(model respOpenwashingLog) app.OpenwashingLog {
	return app.OpenwashingLog{
		ID:        model.ID,
		Text:      model.Text,
		StationID: app.StationID(model.StationID),
		Type:      model.Type,
		Level:     appLogLevel(model.Level),
		CreatedAt: model.CreatedAt,
	}
}

func appOpenwashingLogs(models []respOpenwashingLog) []app.OpenwashingLog {
	res := []app.OpenwashingLog{}
	for _, model := range models {
		res = append(res, appOpenwashingLog(model))
	}
	return res
}

func appListTasks(tasks []resTasks) []app.Task {
	var appTasks []app.Task

	for i := 0; i < len(tasks); i++ {
		appTasks = append(appTasks, appTaskForTasks(tasks[i]))
	}

	return appTasks
}

func appTaskForTasks(task resTasks) app.Task {
	return app.Task{
		ID:         task.ID,
		StationID:  app.StationID(task.StationID),
		VersionID:  task.VersionID,
		Type:       appTaskType(task.Type),
		Status:     appTaskStatus(task.Status),
		RetryCount: task.RetryCount,
		Error:      task.Error,
		CreatedAt:  task.CreatedAt,
		StartedAt:  task.StartedAt,
		StoppedAt:  task.StoppedAt,
	}
}

func appTask(task resTask) app.Task {
	return app.Task{
		ID:         task.ID,
		StationID:  app.StationID(task.StationID),
		VersionID:  task.VersionID,
		Type:       appTaskType(task.Type),
		Status:     appTaskStatus(task.Status),
		RetryCount: task.RetryCount,
		Error:      task.Error,
		CreatedAt:  task.CreatedAt,
		StartedAt:  task.StartedAt,
		StoppedAt:  task.StoppedAt,
	}
}

func appTaskType(taskType TaskType) app.TaskType {
	switch taskType {
	case BuildTaskType:
		return app.BuildTaskType
	case UpdateTaskType:
		return app.UpdateTaskType
	case RebootTaskType:
		return app.RebootTaskType
	case GetVersionsTaskType:
		return app.GetVersionsTaskType
	case PullFirmwareTaskType:
		return app.PullFirmwareTaskType
	case SetVersionTaskType:
		return app.SetVersionTaskType
	default:
		panic("Unknown task type: " + taskType)
	}
}

func appTaskStatus(taskStatus TaskStatus) app.TaskStatus {
	switch taskStatus {
	case QueueTaskStatus:
		return app.QueueTaskStatus
	case StartedTaskStatus:
		return app.StartedTaskStatus
	case CompletedTaskStatus:
		return app.CompletedTaskStatus
	case ErrorTaskStatus:
		return app.ErrorTaskStatus
	case CanceledTaskStatus:
		return app.CanceledTaskStatus
	default:
		panic("Unknown task status: " + taskStatus)
	}
}

func appLogLevel(level LogLevel) app.LogLevel {
	switch level {
	case DebugLogLevel:
		return app.DebugLogLevel
	case InfoLogLevel:
		return app.InfoLogLevel
	case WarningLogLevel:
		return app.WarningLogLevel
	case ErrorLogLevel:
		return app.ErrorLogLevel
	default:
		panic("Unknown log level: " + level)
	}
}

func dalLogLevel(level app.LogLevel) LogLevel {
	switch level {
	case app.DebugLogLevel:
		return DebugLogLevel
	case app.InfoLogLevel:
		return InfoLogLevel
	case app.WarningLogLevel:
		return WarningLogLevel
	case app.ErrorLogLevel:
		return ErrorLogLevel
	default:
		panic("Unknown log level: " + level)
	}
}

func dalTaskType(taskType app.TaskType) TaskType {
	switch taskType {
	case app.BuildTaskType:
		return BuildTaskType
	case app.UpdateTaskType:
		return UpdateTaskType
	case app.RebootTaskType:
		return RebootTaskType
	case app.GetVersionsTaskType:
		return GetVersionsTaskType
	case app.PullFirmwareTaskType:
		return PullFirmwareTaskType
	case app.SetVersionTaskType:
		return SetVersionTaskType
	default:
		panic("Unknown task type: " + taskType)
	}
}

func dalTaskStatus(taskStatus app.TaskStatus) TaskStatus {
	switch taskStatus {
	case app.QueueTaskStatus:
		return QueueTaskStatus
	case app.StartedTaskStatus:
		return StartedTaskStatus
	case app.CompletedTaskStatus:
		return CompletedTaskStatus
	case app.ErrorTaskStatus:
		return ErrorTaskStatus
	case app.CanceledTaskStatus:
		return CanceledTaskStatus
	default:
		panic("Unknown task status: " + taskStatus)
	}
}

func dalStationsId(stationsId []app.StationID) pq.Int32Array {
	if stationsId == nil {
		return nil
	}
	ids := []int32{}
	for _, i := range stationsId {
		ids = append(ids, int32(i))
	}
	return ids
}

func dalTaskStatuses(taskStatuses []app.TaskStatus) pq.StringArray {
	if taskStatuses == nil {
		return nil
	}
	statuses := []string{}
	for _, s := range taskStatuses {
		statuses = append(statuses, string(dalTaskStatus(s)))
	}
	return statuses
}

func dalTaskTypes(taskTypes []app.TaskType) pq.StringArray {
	if taskTypes == nil {
		return nil
	}
	statuses := []string{}
	for _, s := range taskTypes {
		statuses = append(statuses, string(dalTaskType(s)))
	}
	return statuses
}

func dalNullableTaskStatus(taskStatus *app.TaskStatus) *TaskStatus {
	if taskStatus == nil {
		return nil
	}
	var dalTaskStatus = dalTaskStatus(*taskStatus)
	return &dalTaskStatus
}

func dalTaskSort(taskStatus *app.TaskSort) *TaskSort {
	if taskStatus == nil {
		return nil
	}
	switch *taskStatus {
	case app.CreatedAtAscTaskSort:
		c := CreatedAtAscTaskSort
		return &c
	case app.CreatedAtDescTaskSort:
		c := CreatedAtDescTaskSort
		return &c
	default:
		panic("Unknown task status: " + *taskStatus)
	}
}
