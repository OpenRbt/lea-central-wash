package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

func apiRelayReport(data app.RelayReport) *model.RelayReport {
	var relayStats []*model.RelayStat
	for i := 1; i <= 6; i++ {
		r := model.RelayStat{
			RelayID:       data.RelayStats[i].RelayID,
			SwitchedCount: data.RelayStats[i].SwitchedCount,
			TotalTimeOn:   data.RelayStats[i].TotalTimeOn,
		}
		relayStats = append(relayStats, &r)
	}
	return &model.RelayReport{
		Hash:       data.Hash,
		RelayStats: relayStats,
	}
}

func apiMoneyReport(data app.MoneyReport) *model.MoneyReport {
	return &model.MoneyReport{
		Hash:         data.Hash,
		Banknotes:    data.Banknotes,
		CarsTotal:    data.CarsTotal,
		Coins:        data.Coins,
		Electronical: data.Electronical,
		Service:      data.Service,
	}
}
