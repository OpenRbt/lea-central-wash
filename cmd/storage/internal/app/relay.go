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

func (a *app) MeasureVolumeMilliliters(volume int64, id StationID, StartProgramID int64, StopProgramID int64) (err error) {
	cfg1 := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if StartProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[StartProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		cfg1.MotorSpeedPercent = int(program.MotorSpeedPercent)
		cfg1.Timings = program.Relays
	}

	cfg2 := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if StopProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[StopProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		cfg2.MotorSpeedPercent = int(program.MotorSpeedPercent)
		cfg2.Timings = program.Relays
	}

	fmt.Println(cfg1.Timings)
	fmt.Println(cfg2.Timings)

	return a.hardware.MeasureVolumeMilliliters(volume, id, cfg1, cfg2)
}

func (a *app) GetVolumeDispenser() (znach int64, status string, err error) {
	zhach, status, err := a.hardware.Volume()
	return zhach, status, err
}

func (a *app) GetLevel() (level int64, err error) {
	level, err = a.hardware.GetLevel()
	return level, nil
}

func (a *app) DispenserStop(id StationID, StopProgramID int64) (err error) {
	cfg := RelayConfig{
		TimeoutSec: relayTimeoutSec,
	}
	if StopProgramID > 0 {
		a.programsMutex.Lock()
		program, ok := a.programs[StopProgramID]
		a.programsMutex.Unlock()
		if !ok {
			return ErrNotFound
		}
		cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
		cfg.Timings = program.Relays
	}
	return a.hardware.DispenserStop(id, cfg)
}
