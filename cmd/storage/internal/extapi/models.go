package extapi

import (
	"fmt"

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
		Hash:       model.Hash(data.Hash),
		RelayStats: relayStats,
	}
}

func apiMoneyReport(data *app.MoneyReport) *model.MoneyReport {
	return &model.MoneyReport{
		Hash:         model.Hash(data.Hash),
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

func apiStatusReport(v app.StatusReport) *model.StatusReport {
	var stationStatus []*model.StationStatus
	for i := range v.Stations {
		stationStatus = append(stationStatus, apiStationStatus(v.Stations[i]))
	}
	return &model.StatusReport{
		KasseInfo:   v.KasseInfo,
		KasseStatus: apiStatus(v.KasseStatus),
		LcwInfo:     v.LCWInfo,
		Stations:    stationStatus,
	}
}

func apiStationStatus(v app.StationStatus) *model.StationStatus {
	return &model.StationStatus{
		Hash:   model.Hash(v.Hash),
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

func apiStationsKeyPair(data []app.StationKeyPair) []*model.StationKeyPair {
	var res []*model.StationKeyPair
	for i := range data {
		res = append(res, &model.StationKeyPair{
			ID:       int64(data[i].ID),
			Hash:     data[i].Hash,
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
