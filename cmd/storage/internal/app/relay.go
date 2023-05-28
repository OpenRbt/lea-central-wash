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

func (a *app) MeasureVolumeMilliliters(volume int64, stationID StationID, startProgramID int64, stopProgramID int64) (err error) {
	startCfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if startProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[startProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		startCfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
		startCfg.Timings = program.Relays
	}

	stopCfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if stopProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[stopProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		stopCfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
		stopCfg.Timings = program.Relays
	}

	a.stationsMutex.Lock()
	vCorr := a.volumeCorrection
	a.stationsMutex.Unlock()
	v := volume * int64(vCorr) / 1000

	return a.hardware.MeasureVolumeMilliliters(v, stationID, startCfg, stopCfg)
}

func (a *app) GetVolumeDispenser() (volume int64, status string, err error) {
	getVolume, status, err := a.hardware.Volume()
	a.stationsMutex.Lock()
	vCorr := a.volumeCorrection
	a.stationsMutex.Unlock()
	volume = int64(float64(getVolume) / float64(vCorr) * 1000)
	return volume, status, err
}

// GetLevel returnы liquid level
func (a *app) GetLevel() (level int64, err error) {
	level, err = a.hardware.GetLevel()
	if err != nil {
		return level, nil
	}
	return level, err
}

func (a *app) DispenserStop(stationID StationID, stopProgramID int64) (err error) {
	cfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if stopProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[stopProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
		cfg.Timings = program.Relays
	}
	return a.hardware.DispenserStop(stationID, cfg)
}
