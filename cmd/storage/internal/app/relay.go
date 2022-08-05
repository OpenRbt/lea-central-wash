package app

import "fmt"

func (a *app) RunProgram(id StationID, programID int64, preflight bool) (err error) {
	fmt.Println("Start Run1programm")
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
	for _, relay := range cfg.Timings {
		fmt.Println(relay.ID, " ", relay.TimeOff, " ", relay.TimeOn)
	}
	return nil
	// return a.hardware.RunProgram(int32(id), cfg)
}

func (a *app) Run2Program(id StationID, programID int64, programID2 int64, preflight bool) (err error) {
	fmt.Println("Start Run2programm")
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

		if preflight {
			cfg.MotorSpeedPercent = int(program.PreflightMotorSpeedPercent)
			cfg.Timings = program.PreflightRelays
			cfg.Timings = append(cfg.Timings, program2.Relays...)
		} else {
			cfg.MotorSpeedPercent = int(program.MotorSpeedPercent)
			cfg.Timings = program.Relays
			cfg.Timings = append(cfg.Timings, program2.Relays...)
		}
	}
	for _, relay := range cfg.Timings {
		fmt.Println(relay.ID, " ", relay.TimeOff, " ", relay.TimeOn)
	}
	return nil
	//return a.hardware.RunProgram(int32(id), cfg)
}
