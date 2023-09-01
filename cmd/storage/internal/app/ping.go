package app

import "time"

// Ping sets the time of the last ping and returns service money.
func (a *app) Ping(id StationID, balance, program int, stationIP string) (StationData, bool) {
	a.stationsMutex.RLock()
	var station StationData
	if v, ok := a.stations[id]; ok {
		station = v
	} else {
		station = StationData{}
	}
	a.stationsMutex.RUnlock()
	oldStation := station
	station.LastPing = time.Now()
	station.ServiceMoney = 0
	station.BonusMoney = 0
	station.OpenStation = false
	station.CurrentBalance = balance
	station.CurrentProgram = program
	station.ButtonID = 0
	station.IP = stationIP
	station.RunProgram = oldStation.RunProgram
	if oldStation.CurrentProgram != station.CurrentProgram {
		station.RunProgram = time.Now()
		if oldStation.CurrentProgram > 0 {
			go a.saveStationStat(id, oldStation.CurrentProgram, time.Since(oldStation.RunProgram))
		}
	}
	a.stationsMutex.Lock()
	a.stations[id] = station
	a.stationsMutex.Unlock()

	oldStation.LastUpdate = a.lastUpdate
	oldStation.LastDiscountUpdate = a.lastDiscountUpdate

	bonusSystemActive := false
	if a.bonusSystemRabbitWorker != nil {
		bonusSystemActive = a.bonusSystemRabbitWorker.IsConnected()
	}

	return oldStation, bonusSystemActive
}
