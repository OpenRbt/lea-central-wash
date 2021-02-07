package extapi

import (
	"fmt"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

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
		Hash:   model.Hash(svc.getHash(v.ID)),
		ID:     int64(v.ID),
		Info:   v.Info,
		Name:   v.Name,
		Status: apiStatus(v.Status),
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

func apiPrograms(p []app.Program) (res []*model.ProgramInfo) {
	res = []*model.ProgramInfo{}
	for i := range p {
		res = append(res, &model.ProgramInfo{
			ID:   int64(p[i].ID),
			Name: p[i].Name,
		})
	}
	return res
}

func apiRelays(r []app.Relay) (res []*model.RelayConfig) {
	res = []*model.RelayConfig{}
	for i := range r {
		res = append(res, &model.RelayConfig{
			ID:        int64(r[i].ID),
			Timeon:    int64(r[i].TimeOn),
			Timeoff:   int64(r[i].TimeOff),
			Prfelight: int64(r[i].Preflight),
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

func apiUserReport(v app.UserData) *model.UserConfig {
	return &model.UserConfig{
		Login:      &v.Login,
		FirstName:  &v.FirstName,
		MiddleName: &v.MiddleName,
		LastName:   &v.LastName,
		IsAdmin:    &v.IsAdmin,
		IsOperator: &v.IsOperator,
		IsEngineer: &v.IsEngineer,
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
