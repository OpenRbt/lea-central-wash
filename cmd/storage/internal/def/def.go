// Package def provides default values for both commands and tests.
package def

import (
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/storageapi/restapi"
	"github.com/go-openapi/loads"
	"github.com/go-openapi/swag"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/powerman/must"
	"github.com/powerman/sqlxx"
	"github.com/powerman/structlog"
)

// Log field names.
const (
	LogHost       = "host"
	LogPort       = "port"
	LogAddr       = "addr"
	LogRemote     = "remote" // aligned IPv4:Port "   192.168.0.42:1234 "
	LogFunc       = "func"   // RPC method name, REST resource path
	LogHTTPMethod = "httpMethod"
	LogHTTPStatus = "httpStatus"
)

// Default values.
var (
	oapiHost, _, oapiBasePath = swaggerEndpoint()

	TestTimeFactor     = floatGetEnv("GO_TEST_TIME_FACTOR", 1.0)
	TestSecond         = time.Duration(float64(time.Second) * TestTimeFactor)
	DBHost             = strGetEnv("STORAGE_DB_HOST", "")
	DBPort             = intGetEnv("STORAGE_DB_PORT", 5432)
	DBUser             = strGetEnv("STORAGE_DB_USER", "")
	DBPass             = strGetEnv("STORAGE_DB_PASS", "")
	DBName             = strGetEnv("STORAGE_DB_NAME", "")
	DBSchema           = strGetEnv("STORAGE_DB_SCHEMA", "public")
	GooseDir           = strGetEnv("GOOSE_DIR", "cmd/storage/internal/migration")
	ExtAPIHost         = strGetEnv("STORAGE_EXT_HOST", oapiHost)
	ExtAPIPort         = intGetEnv("STORAGE_EXT_PORT", 8020)
	ExtAPIBasePath     = strGetEnv("STORAGE_EXT_BASEPATH", oapiBasePath)
	KasseEndpoint      = strGetEnv("STORAGE_KASSE_ENDPOINT", "https://localhost:8443")
	HALEndpoint        = strGetEnv("STORAGE_HAL_ENDPOINT", ":8099")
	MeteoInfoBaseURL   = "https://meteoinfo.ru/pogoda"
	OpenWeatherBaseURL = "http://api.openweathermap.org/data/2.5/weather"
	OpenWeatherAPIKey  = strGetEnv("OPENWEATHER_API_KEY", "")
	IpifyBaseURL       = "https://geo.ipify.org/api/v1"
	IpifyAPIKey        = strGetEnv("IPIFY_API_KEY", "")
	CleanupTimeout     = intGetEnv("STORAGE_API_CLEANUP_TIMEOUT", 5)
	ReadTimeout        = intGetEnv("STORAGE_API_READ_TIMEOUT", 2)
	WriteTimeout       = intGetEnv("STORAGE_API_WRITE_TIMEOUT", 2)
	StartDelaySec      = intGetEnv("STORAGE_START_DELAY", 30)
	OpenwashingURL     = strGetEnv("OPENWASHING_URL", "https://app.openrbt.com")

	RabbitHost = strGetEnv("RABBIT_HOST", "app.openrbt.com")
	RabbitPort = strGetEnv("RABBIT_PORT", "4043")

	// sbp
	SbpRabbitHost              = strGetEnv("SBP_RABBIT_HOST", "app.openrbt.com")
	SbpRabbitPort              = strGetEnv("SBP_RABBIT_PORT", "4043")
	SbpRabbitSecure            = boolGetEnv("SBP_RABBIT_SECURE", true)
	SbpPaymentExpirationPeriod = durationGetEnv("SBP_PAYMENT_EXPIRATION_PERIOD", time.Minute*5)
	SbpEnvNameServerID         = strGetEnv("SBP_ENV_NAME_SERVER_ID", "SBP_SERVER_ID")
	SbpEnvNameServerPassword   = strGetEnv("SBP_ENV_NAME_SERVER_PASSWORD", "SBP_SERVER_PASSWORD")
	//

	MngtRabbitHost   = strGetEnv("MNGT_RABBIT_HOST", "dev.openrbt.com")
	MngtRabbitPort   = strGetEnv("MNGT_RABBIT_PORT", "4043")
	MngtRabbitSecure = boolGetEnv("MNGT_RABBIT_SECURE", true)
)

var initErr error

var Offset int

// Init must be called once before using this package.
// It provides common initialization for both commands and tests.
func Init() error {
	_, Offset = time.Now().Zone()
	time.Local = time.UTC
	must.AbortIf = must.PanicIf
	sqlx.NameMapper = sqlxx.ToSnake
	structlog.DefaultLogger.
		AppendPrefixKeys(
			LogRemote,
			LogHTTPStatus,
			LogHTTPMethod,
			LogFunc,
		).
		SetSuffixKeys(
			structlog.KeyStack,
		).
		SetKeysFormat(map[string]string{
			structlog.KeyUnit: " %7[2]s:", // set to max KeyUnit/package length
			LogHost:           " %[2]s",
			LogPort:           ":%[2]v",
			LogAddr:           " %[2]s",
			LogRemote:         " %-21[2]s",
			LogHTTPStatus:     " %3[2]v",
			LogHTTPMethod:     "%.0[2]s", // disabled, to enable: " %-7[2]s",
			LogFunc:           " %[2]s:",
			"version":         " %s %v",
			"err":             " %s: %v",
			"json":            " %s=%#q",
			"ptr":             " %[2]p", // for debugging references
		})
	return initErr
}

func floatGetEnv(name string, def float64) float64 {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	v, err := strconv.ParseFloat(value, 64)
	if err != nil {
		initErr = errors.Errorf("failed to parse %q=%q as float: %v", name, value, err)
		return def
	}
	return v
}

func intGetEnv(name string, def int) int {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	i, err := strconv.Atoi(value)
	if err != nil {
		initErr = errors.Errorf("failed to parse %q=%q as int: %v", name, value, err)
		return def
	}
	return i
}

func strGetEnv(name, def string) string {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	return value
}

func boolGetEnv(name string, def bool) bool {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	v, err := strconv.ParseBool(value)
	if err != nil {
		initErr = errors.Errorf("failed to parse %q=%q as bool: %v", name, value, err)
		return def
	}
	return v
}

func durationGetEnv(name string, def time.Duration) time.Duration {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	d, err := time.ParseDuration(value)
	if err != nil {
		initErr = errors.Errorf("failed to parse %q=%q as duration: %v", name, value, err)
		return def
	}
	return d
}

func swaggerEndpoint() (host string, port int, basePath string) {
	const portHTTP = 80
	const portHTTPS = 443

	spec, err := loads.Embedded(restapi.SwaggerJSON, restapi.FlatSwaggerJSON)
	if err != nil {
		return "", 0, ""
	}

	host, port, err = swag.SplitHostPort(spec.Host())
	switch {
	case err == nil:
		return host, port, spec.BasePath()
	case strings.Contains(err.Error(), "missing port"):
		schemes := spec.Spec().Schemes
		switch {
		case len(schemes) == 1 && schemes[0] == "http":
			return spec.Host(), portHTTP, spec.BasePath()
		case len(schemes) == 1 && schemes[0] == "https":
			return spec.Host(), portHTTPS, spec.BasePath()
		}
	}
	return spec.Host(), 0, spec.BasePath()
}
