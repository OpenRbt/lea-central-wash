package extapi

import (
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
