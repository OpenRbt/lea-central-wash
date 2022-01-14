package app

const (
	DefaultTimeZone = 7 * 60
)

var (
	AppCfg = AppConfig{}
)

type (
	AppConfig struct {
		TimeZone *ConfigInt
	}
)

func CheckConfig() error {
	return nil
}

func LoadConfig() error {
	return nil
}
