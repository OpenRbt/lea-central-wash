package extapi

import (
	"fmt"
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
	return app.Program{
		ID:                         *p.ID,
		Name:                       p.Name,
		Price:                      int(p.Price),
		PreflightEnabled:           p.PreflightEnabled,
		MotorSpeedPercent:          *p.MotorSpeedPercent,
		PreflightMotorSpeedPercent: *p.PreflightMotorSpeedPercent,
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
		Hash:           model.Hash(svc.getHash(v.ID)),
		ID:             int64(v.ID),
		Info:           v.Info,
		Name:           v.Name,
		Status:         apiStatus(v.Status),
		CurrentBalance: int64(v.CurrentBalance),
		CurrentProgram: int64(v.CurrentProgram),
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

func apiButtons(r []app.StationProgram) (res []*op.ButtonsItems0) {
	res = []*op.ButtonsItems0{}
	for i := range r {
		res = append(res, &op.ButtonsItems0{
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
		Host:           v.Host,
		Port:           v.Port,
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
		Login:      model.Login(v.Login),
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
	res.RelayBoard = model.RelayBoard(p.RelayBoard)
	for i := range p.Programs {
		res.Programs = append(res.Programs, &model.StationProgramsProgramsItems0{
			ButtonID: int64(p.Programs[i].ButtonID),
			Program:  apiProgram(p.Programs[i]),
		})
	}
	return res
}
