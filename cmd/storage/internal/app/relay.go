package app

func (a *app) RunProgram(id StationID, programID int64, preflight bool) (err error) {
	cfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if programID > 0 {
		program, err := a.repo.Programs(&programID)
		if err != nil {
			return err
		}
		if len(program) != 1 {
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
			cfg.MotorSpeedPercent = int(program[0].PreflightMotorSpeedPercent)
			cfg.Timings = program[0].PreflightRelays
		} else {
			cfg.MotorSpeedPercent = int(program[0].MotorSpeedPercent)
			cfg.Timings = program[0].Relays
		}
	}
	return a.hardware.RunProgram(int(id), cfg)
}
