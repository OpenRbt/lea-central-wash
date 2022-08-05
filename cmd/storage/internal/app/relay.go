package app

import "fmt"

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
	if programID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[programID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}

		a.programsMutex.Lock()
		program2, ok := a.programs[programID2]
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

		list := []Relay{}

		if preflight {
			cfg.MotorSpeedPercent = int(program.PreflightMotorSpeedPercent)
			list = program.PreflightRelays
			list = append(list, program2.Relays...)
			// cfg.Timings = program.PreflightRelays
			// cfg.Timings = append(cfg.Timings, program2.Relays...)
		} else {
			cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
			list = program.PreflightRelays
			list = append(list, program2.Relays...)
			// cfg.Timings = program.Relays
			// cfg.Timings = append(cfg.Timings, program2.Relays...)
		}
		keys := make(map[Relay]bool)

		for _, entry := range list {
			if _, value := keys[entry]; !value {
				keys[entry] = true
				cfg.Timings = append(cfg.Timings, entry)
			}
		}
	}
	fmt.Println("Config is completeS")
	return a.hardware.RunProgram(int32(id), cfg)
}
