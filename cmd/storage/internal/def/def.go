// Package def provides default values for both commands and tests.
package def

import (
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi"
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

	TestTimeFactor     = floatGetenv("GO_TEST_TIME_FACTOR", 1.0)
	TestSecond         = time.Duration(float64(time.Second) * TestTimeFactor)
	DBHost             = strGetenv("STORAGE_DB_HOST", "")
	DBPort             = intGetenv("STORAGE_DB_PORT", 5432)
	DBUser             = strGetenv("STORAGE_DB_USER", "")
	DBPass             = strGetenv("STORAGE_DB_PASS", "")
	DBName             = strGetenv("STORAGE_DB_NAME", "")
	DBSchema           = strGetenv("STORAGE_DB_SCHEMA", "public")
	GooseDir           = strGetenv("GOOSE_DIR", "cmd/storage/internal/migration")
	ExtAPIHost         = strGetenv("STORAGE_EXT_HOST", oapiHost)
	ExtAPIPort         = intGetenv("STORAGE_EXT_PORT", 8020)
	ExtAPIBasePath     = strGetenv("STORAGE_EXT_BASEPATH", oapiBasePath)
	KasseEndpoint      = strGetenv("STORAGE_KASSE_ENDPOINT", "https://localhost:8443")
	HALEndpoint        = strGetenv("STORAGE_HAL_ENDPOINT", ":8099")
	MeteoInfoBaseURL   = "https://meteoinfo.ru/pogoda"
	OpenWeatherBaseURL = "http://api.openweathermap.org/data/2.5/weather"
	OpenWeatherAPIKey  = strGetenv("OPENWEATHER_API_KEY", "")
	IpifyBaseURL       = "https://geo.ipify.org/api/v1"
	IpifyAPIKey        = strGetenv("IPIFY_API_KEY", "")
	CleanupTimeout     = intGetenv("STORAGE_API_CLEANUP_TIMEOUT", 5)
	ReadTimeout        = intGetenv("STORAGE_API_READ_TIMEOUT", 2)
	WriteTimeout       = intGetenv("STORAGE_API_WRITE_TIMEOUT", 2)
	StartDelaySec      = intGetenv("STORAGE_START_DELAY", 30)
	OpenwashingURL     = strGetenv("OPENWASHING_URL", "http://app.openwashing.com")

	RabbitHost     = strGetenv("RABBIT_HOST", "localhost")
	RabbitPort     = strGetenv("RABBIT_PORT", "5671")
	RabbitUser     = strGetenv("RABBIT_USER", "")
	RabbitPassword = strGetenv("RABBIT_PASSWORD", "")

	RabbitCertPath = strGetenv("RABBIT_CERT_PATH", "certificates/")
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

func floatGetenv(name string, def float64) float64 {
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

func intGetenv(name string, def int) int {
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

func strGetenv(name, def string) string {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	return value
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
