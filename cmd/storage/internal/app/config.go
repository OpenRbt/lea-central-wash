package app

import (
	"errors"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	uuid "github.com/satori/go.uuid"
)

type (
	AppConfig struct {
		TimeZone        ConfigInt
		BonusServiceURL string
	}
)

func CheckConfig() error {
	return nil
}

func (a *app) loadConfig() error {
	timezone, err := a.repo.GetConfigInt(parameterNameTimeZone)
	if err != nil {
		return err
	}
	cfg := AppConfig{
		TimeZone:        timezone,
		BonusServiceURL: def.OpenwashingURL,
	}
	a.cfgMutex.Lock()
	defer a.cfgMutex.Unlock()
	a.cfg = cfg
	return nil
}

func (a *app) setDefaultConfig() error {
	offset := def.Offset
	log.Info("time", "time", time.Now(), "offset", int64(offset/60))

	err := a.repo.SetConfigIntIfNotExists(ConfigInt{
		Name:        parameterNameTimeZone,
		Description: "time zone",
		Value:       int64(offset / 60),
	})
	if err != nil {
		return err
	}

	if _, err = a.repo.GetConfigString(ParameterNameWashID); err != nil && !errors.Is(err, ErrNotFound) {
		return err
	} else if err != nil {
		err = a.repo.SetConfigString(ConfigString{
			Name:        ParameterNameWashID,
			Description: ParameterNameWashID,
			Note:        ParameterNameWashID,
			Value:       uuid.NewV4().String(),
		})
		if err != nil {
			return err
		}
	}

	return nil
}

func (a *app) GetWashID() (string, error) {
	config, err := a.repo.GetConfigString(ParameterNameWashID)
	if err != nil {
		return "", err
	}

	return config.Value, nil
}

func (a *app) GetConfigInt(auth *Auth, name string) (ConfigInt, error) {
	return a.repo.GetConfigInt(name)
}

func (a *app) GetConfigBool(auth *Auth, name string) (ConfigBool, error) {
	return a.repo.GetConfigBool(name)
}

func (a *app) GetConfigString(auth *Auth, name string) (ConfigString, error) {
	return a.repo.GetConfigString(name)
}

func (a *app) SetConfigInt(auth *Auth, config ConfigInt) error {
	return a.signalAfterUpdateConfig(a.repo.SetConfigInt(config))
}

func (a *app) SetConfigBool(auth *Auth, config ConfigBool) error {
	return a.signalAfterUpdateConfig(a.repo.SetConfigBool(config))
}

func (a *app) SetConfigString(auth *Auth, config ConfigString) error {
	return a.signalAfterUpdateConfig(a.repo.SetConfigString(config))
}

func (a *app) DeleteConfigString(auth *Auth, name string) error {
	return a.signalAfterUpdateConfig(a.repo.DeleteConfigString(name))
}

func (a *app) GetStationConfigInt(name string, stationID StationID) (StationConfigVar[int64], error) {
	return a.repo.GetStationConfigInt(name, stationID)
}

func (a *app) GetStationConfigBool(name string, stationID StationID) (StationConfigVar[bool], error) {
	return a.repo.GetStationConfigBool(name, stationID)
}

func (a *app) GetStationConfigString(name string, stationID StationID) (StationConfigVar[string], error) {
	return a.repo.GetStationConfigString(name, stationID)
}

func (a *app) SetStationConfigInt(auth *Auth, config StationConfigVar[int64]) error {
	if config.Name == ParameterNameVolumeCoef {
		a.stationsMutex.Lock()
		a.volumeCorrection = int(config.Value)
		a.stationsMutex.Unlock()
	}
	return a.signalAfterUpdateConfig(a.repo.SetStationConfigInt(config))
}

func (a *app) SetStationConfigBool(auth *Auth, config StationConfigVar[bool]) error {
	return a.signalAfterUpdateConfig(a.repo.SetStationConfigBool(config))
}

func (a *app) SetStationConfigString(auth *Auth, config StationConfigVar[string]) error {
	return a.signalAfterUpdateConfig(a.repo.SetStationConfigString(config))
}

func (a *app) signalAfterUpdateConfig(err error) error {
	if err != nil {
		return err
	}
	a.sendManagementSyncSignal()
	return nil
}
