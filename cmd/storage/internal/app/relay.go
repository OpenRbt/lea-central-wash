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
	return a.hardware.RunProgram(int64(id), cfg)
}
