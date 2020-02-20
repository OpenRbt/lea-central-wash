package extapi

import (
	"fmt"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

func apiRelayReport(hash model.Hash) *model.RelayReport {
	var relayStats []*model.RelayStat
	for i := 1; i <= 6; i++ {
		r := model.RelayStat{
			RelayID:       int64(i),
			SwitchedCount: int64(5 * i),
			TotalTimeOn:   int64(10 * i),
		}
		relayStats = append(relayStats, &r)
	}
	return &model.RelayReport{
		Hash:       hash,
		RelayStats: relayStats,
	}
}

func apiMoneyReport(hash model.Hash) *model.MoneyReport {
	return &model.MoneyReport{
		Hash:         hash,
		Banknotes:    25,
		CarsTotal:    100,
		Coins:        150,
		Electronical: 0,
		Service:      5,
	}
}

func apiStatusReport(v app.StatusReport) *model.StatusReport {
	var stationStatus []*model.StationStatus
	for i, _ := range v.Stations {
		stationStatus = append(stationStatus, apiStationStatus(v.Stations[i]))
	}
	return &model.StatusReport{
		KasseInfo:   v.KasseInfo,
		KasseStatus: apiStatus(v.KasseStatus),
		LcwInfo:     v.LcwInfo,
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
