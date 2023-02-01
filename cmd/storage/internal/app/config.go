package app

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/def"
)

type (
	AppConfig struct {
		TimeZone ConfigInt
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
		TimeZone: *timezone,
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
	return err
}

func (a *app) GetConfigInt(auth *Auth, name string) (*ConfigInt, error) {
	return a.repo.GetConfigInt(name)
}

func (a *app) GetConfigBool(auth *Auth, name string) (*ConfigBool, error) {
	return a.repo.GetConfigBool(name)
}

func (a *app) GetConfigString(auth *Auth, name string) (*ConfigString, error) {
	return a.repo.GetConfigString(name)
}

func (a *app) SetConfigInt(auth *Auth, config ConfigInt) error {
	return a.repo.SetConfigInt(config)
}

func (a *app) SetConfigBool(auth *Auth, config ConfigBool) error {
	return a.repo.SetConfigBool(config)
}

func (a *app) SetConfigString(auth *Auth, config ConfigString) error {
	return a.repo.SetConfigString(config)
}

func (a *app) GetLocalConfigInt(auth *Auth, name string) (*LocalConfigInt, error) {
	return a.repo.GetLocalConfigInt(name)
}

func (a *app) GetLocalConfigBool(auth *Auth, name string) (*LocalConfigBool, error) {
	return a.repo.GetLocalConfigBool(name)
}

func (a *app) GetLocalConfigString(auth *Auth, name string) (*LocalConfigString, error) {
	return a.repo.GetLocalConfigString(name)
}

func (a *app) SetLocalConfigInt(auth *Auth, config LocalConfigInt) error {
	return a.repo.SetLocalConfigInt(config)
}

func (a *app) SetLocalConfigBool(auth *Auth, config LocalConfigBool) error {
	return a.repo.SetLocalConfigBool(config)
}

func (a *app) SetLocalConfigString(auth *Auth, config LocalConfigString) error {
	return a.repo.SetLocalConfigString(config)
}
