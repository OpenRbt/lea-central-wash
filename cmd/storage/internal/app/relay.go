package app

func (a *app) RunProgram(id StationID, programID int64, preflight bool) (err error) {
	cfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if programID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[programID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		if preflight {
			station, err := a.repo.Station(id)
			if err != nil {
				return err
			}
			cfg.TimeoutSec = station.PreflightSec + 2
		}

		if preflight {
			cfg.MotorSpeedPercent = int(program.PreflightMotorSpeedPercent)
			cfg.Timings = program.PreflightRelays
		} else {
			cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
			cfg.Timings = program.Relays
		}
	}
	return a.hardware.RunProgram(int32(id), cfg)
}

func (a *app) Run2Program(id StationID, programID int64, programID2 int64, preflight bool) (err error) {
	cfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if programID > 0 || programID2 > 0 {
		program := Program{}
		program2 := Program{}
		if programID > 0 {
			a.programsMutex.Lock()
			p, ok := a.programs[programID]
			program = p
			a.programsMutex.Unlock()
			if !ok {
				return ErrNotFound
			}
		}
		if programID2 > 0 {
			a.programsMutex.Lock()
			p, ok := a.programs[programID2]
			program2 = p
			a.programsMutex.Unlock()
			if !ok {
				return ErrNotFound
			}
		}

		if preflight {
			station, err := a.repo.Station(id)
			if err != nil {
				return err
			}
			cfg.TimeoutSec = station.PreflightSec + 2
		}

		list := []Relay{}

		if preflight {
			cfg.MotorSpeedPercent = int(program.PreflightMotorSpeedPercent)
			list = append(list, program.PreflightRelays...)
			list = append(list, program2.PreflightRelays...)
		} else {
			cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
			list = append(list, program.Relays...)
			list = append(list, program2.Relays...)
		}

		keys := make(map[int]bool)

		for _, entry := range list {
			if _, value := keys[entry.ID]; !value {
				keys[entry.ID] = true
				cfg.Timings = append(cfg.Timings, entry)
			}
		}
	}
	return a.hardware.RunProgram(int32(id), cfg)
}

func (a *app) MeasureVolumeMilliliters(volume int64, stationID StationID) (err error) {
	a.stationsMutex.Lock()
	vCorr := a.volumeCorrection
	a.stationsMutex.Unlock()
	v := volume * int64(vCorr) / 1000
	return a.hardware.MeasureVolumeMilliliters(v)
}

func (a *app) GetVolumeDispenser() (znach int64, status string, err error) {
	zhach, status, err := a.hardware.Volume()
	a.stationsMutex.Lock()
	vCorr := a.volumeCorrection
	a.stationsMutex.Unlock()
	v := zhach / int64(vCorr) * 1000
	return v, status, err
}

func (a *app) GetLevel() (level int64, err error) {
	level, err = a.hardware.GetLevel()
	return level, nil
}
