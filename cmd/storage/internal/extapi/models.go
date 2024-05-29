package extapi

import (
	"fmt"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/storageapi/model"
	"github.com/OpenRbt/lea-central-wash/storageapi/restapi/op"
	"github.com/go-openapi/strfmt"
)

func appRelays(m []*model.RelayConfig) []app.Relay {
	res := []app.Relay{}

	for i := range m {
		res = append(res, app.Relay{
			ID:      int(m[i].ID),
			TimeOn:  int(m[i].Timeon),
			TimeOff: int(m[i].Timeoff),
		})
	}
	return res
}

func appPrograms(p *model.Program) app.Program {
	motorSpeedPercent := int64(0)
	preflightMotorSpeedPercent := int64(0)
	if p.MotorSpeedPercent != nil {
		motorSpeedPercent = *p.MotorSpeedPercent
	}
	if p.PreflightMotorSpeedPercent != nil {
		preflightMotorSpeedPercent = *p.PreflightMotorSpeedPercent
	}
	return app.Program{
		ID:                         *p.ID,
		Name:                       p.Name,
		Price:                      int(p.Price),
		PreflightEnabled:           p.PreflightEnabled,
		MotorSpeedPercent:          motorSpeedPercent,
		PreflightMotorSpeedPercent: preflightMotorSpeedPercent,
		IsFinishingProgram:         p.IsFinishingProgram,
		Relays:                     appRelays(p.Relays),
		PreflightRelays:            appRelays(p.PreflightRelays),
	}
}

func apiPrograms(p []app.Program) (res []*model.Program) {
	res = []*model.Program{}
	for i := range p {
		res = append(res, apiProgram(p[i]))
	}
	return res
}

func apiProgram(p app.Program) *model.Program {
	return &model.Program{
		ID:                         &p.ID,
		Name:                       p.Name,
		Price:                      int64(p.Price),
		PreflightEnabled:           p.PreflightEnabled,
		MotorSpeedPercent:          &p.MotorSpeedPercent,
		PreflightMotorSpeedPercent: &p.PreflightMotorSpeedPercent,
		IsFinishingProgram:         p.IsFinishingProgram,
		Relays:                     apiRelays(p.Relays),
		PreflightRelays:            apiRelays(p.PreflightRelays),
	}
}

func apiRelayReport(data *app.RelayReport) *model.RelayReport {
	var relayStats []*model.RelayStat
	for i := range data.RelayStats {
		r := model.RelayStat{
			RelayID:       int64(data.RelayStats[i].RelayID),
			SwitchedCount: int64(data.RelayStats[i].SwitchedCount),
			TotalTimeOn:   data.RelayStats[i].TotalTimeOn,
		}
		relayStats = append(relayStats, &r)
	}
	return &model.RelayReport{
		RelayStats: relayStats,
	}
}

func apiRelayStats(data []app.RelayStat) []*model.RelayStat {
	var relayStats []*model.RelayStat
	for i := range data {
		r := model.RelayStat{
			RelayID:       int64(data[i].RelayID),
			SwitchedCount: int64(data[i].SwitchedCount),
			TotalTimeOn:   data[i].TotalTimeOn,
		}
		relayStats = append(relayStats, &r)
	}
	return relayStats
}

func apiStationsStat(data app.StationsStat) model.StationsStat {
	var res model.StationsStat
	for i := range data {
		res = append(res, &model.StationStat{
			StationID:    int64(data[i].StationID),
			ProgramStats: apiProgramStat(data[i].ProgramStats),
			PumpTimeOn:   int64(data[i].PumpTimeOn),
			RelayStats:   apiRelayStats(data[i].RelayStats),
		})
	}
	return res
}

func apiProgramStat(data []app.ProgramStat) []*model.ProgramStat {
	var programStats []*model.ProgramStat
	for i := range data {
		r := model.ProgramStat{
			ProgramID:   int64(data[i].ProgramID),
			ProgramName: data[i].ProgramName,
			TimeOn:      int64(data[i].TimeOn),
		}
		programStats = append(programStats, &r)
	}
	return programStats
}

func apiMoneyReport(data *app.MoneyReport) *model.MoneyReport {
	return &model.MoneyReport{
		Banknotes:    int64(data.Banknotes),
		CarsTotal:    int64(data.CarsTotal),
		Coins:        int64(data.Coins),
		Electronical: int64(data.Electronical),
		Service:      int64(data.Service),
		Bonuses:      int64(data.Bonuses),
		QrMoney:      int64(data.QrMoney),
	}
}

func apiStatusCollectionReport(v app.StatusCollection) *model.StatusCollectionReport {
	var stations []*model.CollectionReport

	for i := range v.Stations {
		stations = append(stations, apiCollectionReport(v.Stations[i]))
	}

	return &model.StatusCollectionReport{
		Stations: stations,
	}
}

func apiCollectionReport(v app.CollectionReport) *model.CollectionReport {
	return &model.CollectionReport{
		ID:           int64(v.StationID),
		Banknotes:    int64(v.Banknotes),
		CarsTotal:    int64(v.CarsTotal),
		Coins:        int64(v.Coins),
		Electronical: int64(v.Electronical),
		Service:      int64(v.Service),
		Bonuses:      int64(v.Bonuses),
		QrMoney:      int64(v.QrMoney),
		Ctime:        v.Ctime.Unix(),
	}
}

func (svc *service) apiStatusReport(v app.StatusReport) *model.StatusReport {
	var stationStatus []*model.StationStatus
	for i := range v.Stations {
		stationStatus = append(stationStatus, svc.apiStationStatus(v.Stations[i]))
	}
	for key, value := range svc.unknownHash {
		var status model.Status
		if value.Add(time.Second * 10).After(time.Now()) {
			status = model.StatusOnline
		} else {
			status = model.StatusOffline
		}
		stationStatus = append(stationStatus, &model.StationStatus{
			Hash:   model.Hash(key),
			Status: status,
		})
	}

	return &model.StatusReport{
		KasseInfo:   v.KasseInfo,
		KasseStatus: apiStatus(v.KasseStatus),
		LcwInfo:     v.LCWInfo,
		Stations:    stationStatus,
		SbpStatus:   apiServiceStatus(v.SbpStatus),
		BonusStatus: apiServiceStatus(v.BonusStatus),
	}
}

func apiServiceStatus(v app.ServiceStatus) *model.ServiceStatus {
	unpaidStations := []int64{}
	for key, val := range v.UnpaidStations {
		if val {
			unpaidStations = append(unpaidStations, int64(key))
		}
	}

	status := &model.ServiceStatus{
		Available:        v.Available,
		DisabledOnServer: v.DisabledOnServer,
		IsConnected:      v.IsConnected,
		LastErr:          v.LastErr,
		UnpaidStations:   unpaidStations,
	}
	if v.DateLastErr != nil {
		t := v.DateLastErr.Unix()
		status.DateLastErrUTC = &t
	}
	return status
}

func (svc *service) apiStationStatus(v app.StationStatus) *model.StationStatus {
	return &model.StationStatus{
		Hash:               model.Hash(svc.getHash(v.ID)),
		ID:                 int64(v.ID),
		Info:               v.Info,
		Name:               v.Name,
		Status:             apiStatus(v.Status),
		CurrentBalance:     int64(v.CurrentBalance),
		CurrentProgram:     int64(v.CurrentProgram),
		CurrentProgramName: v.ProgramName,
		IP:                 v.IP,
		Version:            apiFirmwareVersion(v.Version),
	}
}

func apiStatus(v app.Status) model.Status {
	var status model.Status
	switch v {
	case app.StatusOffline:
		status = model.StatusOffline
	case app.StatusOnline:
		status = model.StatusOnline
	default:
		panic(fmt.Sprintf("unknown status %d", v))
	}
	return status
}

func apiStationsVariables(data []app.StationsVariables) []*model.StationsVariables {
	var res []*model.StationsVariables
	for i := range data {
		res = append(res, &model.StationsVariables{
			ID:       int64(data[i].ID),
			Name:     data[i].Name,
			KeyPairs: apiKeyPair(data[i].KeyPair),
		})
	}
	return res
}

func apiKeyPair(data []app.KeyPair) []*model.KeyPair {
	var res []*model.KeyPair
	for i := range data {
		res = append(res, &model.KeyPair{
			Key:   &data[i].Key,
			Value: &data[i].Value,
		})
	}
	return res
}

func apiRelays(r []app.Relay) (res []*model.RelayConfig) {
	res = []*model.RelayConfig{}
	for i := range r {
		res = append(res, &model.RelayConfig{
			ID:      int64(r[i].ID),
			Timeon:  int64(r[i].TimeOn),
			Timeoff: int64(r[i].TimeOff),
		})
	}
	return res
}

func apiButtons(r []app.StationProgram) (res []*op.StationButtonOKBodyButtonsItems0) {
	res = []*op.StationButtonOKBodyButtonsItems0{}
	for i := range r {
		res = append(res, &op.StationButtonOKBodyButtonsItems0{
			ProgramID: int64(r[i].ProgramID),
			ButtonID:  int64(r[i].ButtonID),
		})
	}
	return res
}

func apiKasse(k app.Kasse) (res *model.KasseConfig) {
	res = &model.KasseConfig{
		Cashier:         k.CashierFullName,
		CashierINN:      k.CashierINN,
		ReceiptItemName: k.ReceiptItem,
		Tax:             k.TaxType,
	}

	return res
}

func apiCardReaderConfig(v *app.CardReaderConfig) (res *model.CardReaderConfig) {
	res = &model.CardReaderConfig{
		StationID:      newInt64(int64(v.StationID)),
		CardReaderType: v.CardReaderType,
		Host:           strings.TrimSpace(v.Host),
		Port:           strings.TrimSpace(v.Port),
	}
	return res
}

func apiUserReport(v app.UserData) *model.UserConfig {
	firstName := model.FirstName(*v.FirstName)
	middleName := model.MiddleName(*v.MiddleName)
	lastName := model.LastName(*v.LastName)
	isAdmin := model.IsAdmin(*v.IsAdmin)
	isOperator := model.IsOperator(*v.IsOperator)
	isEngineer := model.IsEngineer(*v.IsEngineer)

	return &model.UserConfig{
		Login:      (*model.Login)(&v.Login),
		FirstName:  &firstName,
		MiddleName: &middleName,
		LastName:   &lastName,
		IsAdmin:    &isAdmin,
		IsOperator: &isOperator,
		IsEngineer: &isEngineer,
	}
}

func apiUsersReport(userData []app.UserData) *model.UsersReport {
	var users []*model.UserConfig

	for u := range userData {
		users = append(users, apiUserReport(userData[u]))
	}

	return &model.UsersReport{
		Users: users,
	}
}

func apiStationConfig(p app.StationConfig) (res *model.StationPrograms) {
	res = &model.StationPrograms{}
	res.StationID = int64(p.ID)
	res.Name = p.Name
	res.PreflightSec = int64(p.PreflightSec)
	res.LastUpdate = int64(p.LastUpdate)
	res.RelayBoard = model.RelayBoard(p.RelayBoard)
	for i := range p.Programs {
		res.Programs = append(res.Programs, &model.StationProgramsProgramsItems0{
			ButtonID: int64(p.Programs[i].ButtonID),
			Program:  apiProgram(p.Programs[i]),
		})
	}
	return res
}

func apiCollectionReportWithUser(reports []app.CollectionReportWithUser) (res []*model.CollectionReportWithUser) {
	res = []*model.CollectionReportWithUser{}
	for i := range reports {
		res = append(res, &model.CollectionReportWithUser{
			ID:           int64(reports[i].StationID),
			Banknotes:    int64(reports[i].Banknotes),
			CarsTotal:    int64(reports[i].CarsTotal),
			Coins:        int64(reports[i].Coins),
			Electronical: int64(reports[i].Electronical),
			Service:      int64(reports[i].Service),
			Bonuses:      int64(reports[i].Bonuses),
			QrMoney:      int64(reports[i].QrMoney),
			Ctime:        reports[i].Ctime.Unix(),
			User:         reports[i].User,
		})
	}
	return res
}

func appDiscountPrograms(a []*model.DiscountProgram) []app.DiscountProgram {
	res := []app.DiscountProgram{}
	for i := range a {
		res = append(res, app.DiscountProgram{
			Discount:  a[i].Discount,
			ProgramID: a[i].ProgramID,
		})
	}
	return res
}

func appAdvertisingCampaign(a *model.AdvertisingCampaign) app.AdvertisingCampaign {
	return app.AdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: appDiscountPrograms(a.DiscountPrograms),
		EndDate:          time.Unix(*a.EndDate, 0),
		EndMinute:        *a.EndMinute,
		ID:               a.ID,
		StartDate:        time.Unix(*a.StartDate, 0),
		StartMinute:      *a.StartMinute,
		Weekday:          a.Weekday,
		Enabled:          a.Enabled,
		Name:             a.Name,
	}
}

func apiDiscountPrograms(a []app.DiscountProgram) []*model.DiscountProgram {
	res := []*model.DiscountProgram{}
	for i := range a {
		res = append(res, &model.DiscountProgram{
			Discount:  a[i].Discount,
			ProgramID: a[i].ProgramID,
		})
	}
	return res
}

func apiAdvertisingCampaign(a app.AdvertisingCampaign) *model.AdvertisingCampaign {
	startDate := a.StartDate.Unix()
	endDate := a.EndDate.Unix()
	return &model.AdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: apiDiscountPrograms(a.DiscountPrograms),
		EndDate:          &endDate,
		EndMinute:        &a.EndMinute,
		ID:               a.ID,
		StartDate:        &startDate,
		StartMinute:      &a.StartMinute,
		Weekday:          a.Weekday,
		Enabled:          a.Enabled,
		Name:             a.Name,
	}
}

func apiAdvertisingCampaigns(a []app.AdvertisingCampaign) model.AdvertisingCampaigns {
	res := []*model.AdvertisingCampaign{}
	for i := range a {
		res = append(res, apiAdvertisingCampaign(a[i]))
	}
	return res
}

func apiStationDiscount(a app.StationDiscount) model.StationDiscounts {
	res := model.StationDiscounts{}

	for _, discount := range a.Discounts {
		res = append(res, &model.ButtonDiscount{ButtonID: discount.ButtonID, Discount: discount.Discount})
	}
	return res
}

func appConfigInt(a *model.ConfigVarInt) app.ConfigInt {
	return app.ConfigInt{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appConfigBool(a *model.ConfigVarBool) app.ConfigBool {
	return app.ConfigBool{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appConfigString(a *model.ConfigVarString) app.ConfigString {
	return app.ConfigString{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func apiConfigBool(a *app.ConfigBool) *model.ConfigVarBool {
	return &model.ConfigVarBool{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func apiConfigInt(a *app.ConfigInt) *model.ConfigVarInt {
	return &model.ConfigVarInt{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func apiConfigString(a *app.ConfigString) *model.ConfigVarString {
	return &model.ConfigVarString{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appStationConfigInt(a *model.StationConfigVarInt) app.StationConfigInt {
	return app.StationConfigInt{
		Name:        *a.Name,
		Value:       *a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   app.StationID(*a.StationID),
	}
}

func appStationConfigBool(a *model.StationConfigVarBool) app.StationConfigBool {
	return app.StationConfigBool{
		Name:        *a.Name,
		Value:       *a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   app.StationID(*a.StationID),
	}
}

func appStationConfigString(a *model.StationConfigVarString) app.StationConfigString {
	return app.StationConfigString{
		Name:        *a.Name,
		Value:       *a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   app.StationID(*a.StationID),
	}
}

func apiStationConfigBool(a *app.StationConfigBool) *model.StationConfigVarBool {
	stID := int64(a.StationID)
	return &model.StationConfigVarBool{
		Name:        &a.Name,
		Value:       &a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   &stID,
	}
}

func apiStationConfigInt(a *app.StationConfigInt) *model.StationConfigVarInt {
	stID := int64(a.StationID)
	return &model.StationConfigVarInt{
		Name:        &a.Name,
		Value:       &a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   &stID,
	}
}

func apiStationConfigString(a *app.StationConfigString) *model.StationConfigVarString {
	stID := int64(a.StationID)
	return &model.StationConfigVarString{
		Name:        &a.Name,
		Value:       &a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   &stID,
	}
}

func apiCreateSession(sessionID string, QR string) *model.Session {
	return &model.Session{
		ID: sessionID,
		QR: QR,
	}
}

func apiGetServerInfo(bonusServiceURL string) *model.ServerInfo {
	return &model.ServerInfo{
		BonusServiceURL: bonusServiceURL,
	}
}

func appTaskStatus(taskStatus model.TaskStatus) app.TaskStatus {
	switch taskStatus {
	case model.TaskStatusQueue:
		return app.QueueTaskStatus
	case model.TaskStatusStarted:
		return app.StartedTaskStatus
	case model.TaskStatusCompleted:
		return app.CompletedTaskStatus
	case model.TaskStatusError:
		return app.ErrorTaskStatus
	case model.TaskStatusCanceled:
		return app.CanceledTaskStatus
	default:
		panic("Unknown task status: " + taskStatus)
	}
}

func appTaskType(taskType model.TaskType) app.TaskType {
	switch taskType {
	case model.TaskTypeBuild:
		return app.BuildTaskType
	case model.TaskTypeUpdate:
		return app.UpdateTaskType
	case model.TaskTypeReboot:
		return app.RebootTaskType
	case model.TaskTypeGetVersions:
		return app.GetVersionsTaskType
	case model.TaskTypePullFirmware:
		return app.PullFirmwareTaskType
	case model.TaskTypeSetVersion:
		return app.SetVersionTaskType
	default:
		panic("Unknown task type: " + taskType)
	}
}

func appTaskSort(taskSort string) *app.TaskSort {
	switch taskSort {
	case "createdAtAsc":
		c := app.CreatedAtAscTaskSort
		return &c
	case "createdAtDesc":
		c := app.CreatedAtDescTaskSort
		return &c
	default:
		panic("Unknown task sort: " + taskSort)
	}
}

func appNullableTaskStatus(taskStatus *string) *app.TaskStatus {
	if taskStatus == nil {
		return nil
	}
	var dalTaskStatus = appTaskStatus(model.TaskStatus(*taskStatus))
	return &dalTaskStatus
}

func apiListTasks(tasks app.TaskPage) *model.TaskPage {
	apiTasks := []*model.Task{}
	for _, t := range tasks.Items {
		task := apiTask(t)
		apiTasks = append(apiTasks, task)
	}
	page := int64(tasks.Page)
	pageSize := int64(tasks.PageSize)
	totalPages := int64(tasks.TotalPages)
	totalItems := int64(tasks.TotalItems)
	return &model.TaskPage{
		Items:      apiTasks,
		Page:       &page,
		PageSize:   &pageSize,
		TotalPages: &totalPages,
		TotalItems: &totalItems,
	}
}

func apiTask(task app.Task) *model.Task {
	id := int64(task.ID)
	stationID := int64(int(task.StationID))
	retryCount := int64(int(task.RetryCount))

	var versionID *int64
	if task.VersionID != nil {
		v := int64(*task.VersionID)
		versionID = &v
	}

	return &model.Task{
		ID:         &id,
		StationID:  &stationID,
		VersionID:  versionID,
		Type:       (*model.TaskType)(&task.Type),
		Status:     (*model.TaskStatus)(&task.Status),
		RetryCount: &retryCount,
		Error:      task.Error,
		CreatedAt:  (*strfmt.DateTime)(&task.CreatedAt),
		StartedAt:  (*strfmt.DateTime)(task.StartedAt),
		StoppedAt:  (*strfmt.DateTime)(task.StoppedAt),
	}
}

func appTaskTypes(types []string) []app.TaskType {
	if types == nil {
		return nil
	}
	appTypes := []app.TaskType{}
	for _, t := range types {
		appTypes = append(appTypes, appTaskType(model.TaskType(t)))
	}
	return appTypes
}

func appTaskStatuses(statuses []string) []app.TaskStatus {
	if statuses == nil {
		return nil
	}
	appStatuses := []app.TaskStatus{}
	for _, t := range statuses {
		appStatuses = append(appStatuses, appTaskStatus(model.TaskStatus(t)))
	}
	return appStatuses
}

func appTasksFilter(params op.GetListTasksParams) app.TasksFilter {
	filter := app.TasksFilter{
		Filter: app.Filter{
			Page:     int(*params.Page),
			PageSize: int(*params.PageSize),
		},
		Types:    appTaskTypes(params.Types),
		Statuses: appTaskStatuses(params.Statuses),
		Sort:     appTaskSort(*params.Sort),
	}
	if params.StationsID != nil {
		stationsId := []app.StationID{}
		for _, v := range params.StationsID {
			stationsId = append(stationsId, app.StationID(v))
		}
		filter.StationsID = stationsId
	}
	return filter
}

func appCreateTask(task model.CreateTask) app.CreateTask {
	var versionID *int
	if task.VersionID != nil {
		v := int(*task.VersionID)
		versionID = &v
	}

	return app.CreateTask{
		VersionID: versionID,
		Type:      appTaskType(*task.Type),
		StationID: app.StationID(*task.StationID),
	}
}

func apiListBuildScripts(buildScripts []app.BuildScript) []*model.BuildScript {
	var apiBuildScripts []*model.BuildScript
	for i := 0; i < len(buildScripts); i++ {
		buildScript := apiBuildScript(buildScripts[i])
		apiBuildScripts = append(apiBuildScripts, buildScript)
	}
	return apiBuildScripts
}

func apiBuildScript(buildScript app.BuildScript) *model.BuildScript {
	id := int64(buildScript.ID)
	stationID := int64(int(buildScript.StationID))
	return &model.BuildScript{
		ID:        &id,
		StationID: &stationID,
		Name:      &buildScript.Name,
		Commangs:  buildScript.Commands,
	}
}

func appSetBuildScript(buildScript model.SetBuildScript) app.SetBuildScript {
	var copyFromStationID *app.StationID
	if buildScript.CopyFromStationID != nil {
		v := app.StationID(int(*buildScript.CopyFromStationID))
		copyFromStationID = &v
	}

	return app.SetBuildScript{
		CopyFromStationID: copyFromStationID,
		StationID:         app.StationID(*buildScript.StationID),
		Name:              *buildScript.Name,
		Commands:          buildScript.Commangs,
	}
}

func apiFirmwareVersion(version *app.FirmwareVersion) *model.FirmwareVersion {
	if version == nil {
		return nil
	}
	id := int64(version.ID)
	return &model.FirmwareVersion{
		ID:         &id,
		IsCurrent:  &version.IsCurrent,
		BuiltAt:    (*strfmt.DateTime)(&version.BuiltAt),
		CommitedAt: (*strfmt.DateTime)(&version.CommitedAt),
		HashBinar:  &version.HashBinar,
		HashEnv:    &version.HashEnv,
		HashLua:    &version.HashLua,
	}
}

func apiListFirmwareVersions(versions []app.FirmwareVersion) []*model.FirmwareVersion {
	var apiVersions []*model.FirmwareVersion
	for i := 0; i < len(versions); i++ {
		version := apiFirmwareVersion(&versions[i])
		apiVersions = append(apiVersions, version)
	}
	return apiVersions
}

func openwashingLogCreateToApp(stationID app.StationID, log model.Log) app.OpenwashingLogCreate {
	return app.OpenwashingLogCreate{
		StationID: stationID,
		Text:      *log.Text,
		Type:      log.Type,
	}
}
