package dal

import (
	"encoding/json"
	"strings"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
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

func appPrograms(p []resPrograms) (res []app.Program) {
	for i := range p {
		res = append(res, app.Program{
			ID:                         p[i].ID,
			Name:                       p[i].Name,
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

func dalAdvertisingCampaign(a app.AdvertisingCampaign) argAdvertisingCampaign {
	bytes, err := json.Marshal(a.DiscountPrograms)
	if err != nil {
		panic(err)
	}

	return argAdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: string(bytes),
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

func appAdvertisingCampaign(a resAdvertisingCampaign) *app.AdvertisingCampaign {
	discountPrograms := []app.DiscountProgram{}
	err := json.Unmarshal([]byte(a.DiscountPrograms), &discountPrograms)
	if err != nil {
		panic(err)
	}
	weekday := []string{}
	if a.Weekday != "" {
		weekday = strings.Split(a.Weekday, ",")
	}
	return &app.AdvertisingCampaign{
		DefaultDiscount:  a.DefaultDiscount,
		DiscountPrograms: discountPrograms,
		EndDate:          a.EndDate,
		EndMinute:        a.EndMinute,
		ID:               a.ID,
		StartDate:        a.StartDate,
		StartMinute:      a.StartMinute,
		Weekday:          weekday,
		Enabled:          a.Enabled,
		Name:             a.Name,
	}
}

func appAdvertisingCampaigns(a []resAdvertisingCampaign) []app.AdvertisingCampaign {
	res := []app.AdvertisingCampaign{}
	for i := range a {
		res = append(res, *appAdvertisingCampaign(a[i]))
	}
	return res
}

func appConfigInt(a resGetConfigInt) *app.ConfigInt {
	return &app.ConfigInt{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appConfigBool(a resGetConfigBool) *app.ConfigBool {
	return &app.ConfigBool{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appConfigString(a resGetConfigString) *app.ConfigString {
	return &app.ConfigString{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
	}
}

func appStationConfigInt(a resGetStationConfigInt) *app.StationConfigInt {
	return &app.StationConfigInt{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   a.StationID,
	}
}

func appStationConfigBool(a resGetStationConfigBool) *app.StationConfigBool {
	return &app.StationConfigBool{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   a.StationID,
	}
}

func appStationConfigString(a resGetStationConfigString) *app.StationConfigString {
	return &app.StationConfigString{
		Name:        a.Name,
		Value:       a.Value,
		Description: a.Description,
		Note:        a.Note,
		StationID:   a.StationID,
	}
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
