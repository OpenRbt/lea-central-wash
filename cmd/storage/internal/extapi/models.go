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
		ID:    int64(v.StationID),
		Money: int64(v.Money),
		Ctime: v.Ctime.Unix(),
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
