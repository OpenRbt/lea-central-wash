package extapi

import (
	"fmt"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
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
	}
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

func apiCreateSession(sessionID string, QR string) *model.Session {
	return &model.Session{
		ID: sessionID,
		QR: QR,
	}
}

func apiRefreshSession(UserID string, receiveAmount int) *model.SessionRefresh {
	return &model.SessionRefresh{
		UserID:        UserID,
		ReceiveAmount: int64(receiveAmount),
	}
}
