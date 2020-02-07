package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/go-openapi/loads"
	"github.com/pkg/errors"
	"github.com/powerman/structlog"
)

var log = structlog.New() //nolint:gochecknoglobals

// Config contains configuration for internal API service.
type Config struct {
	Host     string
	Port     int
	BasePath string
}

type service struct {
	app app.App
}

// NewServer returns Swagger server configured to listen on the TCP network
// address cfg.Host:cfg.Port and handle requests on incoming connections.
func NewServer(appl app.App, cfg Config) (*restapi.Server, error) {
	svc := &service{
		app: appl,
	}

	swaggerSpec, err := loads.Embedded(restapi.SwaggerJSON, restapi.FlatSwaggerJSON)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load embedded swagger spec")
	}
	if cfg.BasePath == "" {
		cfg.BasePath = swaggerSpec.BasePath()
	}
	swaggerSpec.Spec().BasePath = cfg.BasePath
	api := op.NewStorageAPI(swaggerSpec)
	api.Logger = structlog.New(structlog.KeyUnit, "swagger").Printf

	api.LoadHandler = op.LoadHandlerFunc(svc.load)
	api.PingHandler = op.PingHandlerFunc(svc.ping)
	api.SaveHandler = op.SaveHandlerFunc(svc.save)
	api.InfoHandler = op.InfoHandlerFunc(svc.info)

	api.LoadRelayHandler = op.LoadRelayHandlerFunc(svc.loadRelay)
	api.SaveRelayHandler = op.SaveRelayHandlerFunc(svc.saveRelay)

	api.LoadMoneyHandler = op.LoadMoneyHandlerFunc(svc.loadMoney)
	api.SaveMoneyHandler = op.SaveMoneyHandlerFunc(svc.saveMoney)

	server := restapi.NewServer(api)
	server.Host = cfg.Host
	server.Port = cfg.Port

	log.Info("Swagger REST protocol", "version", swaggerSpec.Spec().Info.Version)
	return server, nil
}
