package extapi

import (
	"sync"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/auth"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/go-openapi/loads"
	"github.com/pkg/errors"
	"github.com/powerman/structlog"
)

var log = structlog.New() //nolint:gochecknoglobals

var errNotFound = errors.New("not found")

// Config contains configuration for internal API service.
type Config struct {
	Host           string
	Port           int
	BasePath       string
	WriteTimeout   int
	ReadTimeout    int
	CleanupTimeout int
}

type repo interface {
	LoadHash() ([]app.StationID, []string, error)
	SetHash(id app.StationID, hash string) error
	CheckDB() (ok bool, err error)
}

type service struct {
	app           app.App
	stations      map[string]app.StationID
	unknownHash   map[string]time.Time
	stationsMutex sync.Mutex
	repo          repo
	authAccess    auth.Check
}

// NewServer returns Swagger server configured to listen on the TCP network
// address cfg.Host:cfg.Port and handle requests on incoming connections.
func NewServer(appl app.App, cfg Config, repo repo, authAccess auth.Check) (*restapi.Server, error) {
	svc := &service{
		app:        appl,
		repo:       repo,
		authAccess: authAccess,
	}
	ok, err := svc.repo.CheckDB()
	if err != nil || !ok {
		log.Warn("api check db failed", "ok", ok, "err", err)
		time.Sleep(10 * time.Second)
		panic(1)
	}
	log.Info("api db checked")
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

	api.PinCodeAuth = authAccess.CheckAuth
	api.LoadHandler = op.LoadHandlerFunc(svc.load)
	api.LoadFromStationHandler = op.LoadFromStationHandlerFunc(svc.loadFromStation)
	api.PingHandler = op.PingHandlerFunc(svc.ping)
	api.GetPingHandler = op.GetPingHandlerFunc(svc.getPing)
	api.SaveHandler = op.SaveHandlerFunc(svc.save)
	api.InfoHandler = op.InfoHandlerFunc(svc.info)

	api.LoadRelayHandler = op.LoadRelayHandlerFunc(svc.loadRelay)
	api.SaveRelayHandler = op.SaveRelayHandlerFunc(svc.saveRelay)

	api.LoadMoneyHandler = op.LoadMoneyHandlerFunc(svc.loadMoney)
	api.SaveMoneyHandler = op.SaveMoneyHandlerFunc(svc.saveMoney)

	api.StatusHandler = op.StatusHandlerFunc(svc.status)
	api.StationHandler = op.StationHandlerFunc(svc.station)
	api.SetStationHandler = op.SetStationHandlerFunc(svc.setStation)
	api.DelStationHandler = op.DelStationHandlerFunc(svc.delStation)
	api.StationReportDatesHandler = op.StationReportDatesHandlerFunc(svc.stationReportDates)
	api.StationReportCurrentMoneyHandler = op.StationReportCurrentMoneyHandlerFunc(svc.stationReportCurrentMoney)
	api.AddServiceAmountHandler = op.AddServiceAmountHandlerFunc(svc.addServiceAmount)

	api.StatusCollectionHandler = op.StatusCollectionHandlerFunc(svc.statusCollection)
	api.SaveCollectionHandler = op.SaveCollectionHandlerFunc(svc.saveCollection)
	api.StationCollectionReportDatesHandler = op.StationCollectionReportDatesHandlerFunc(svc.stationCollectionReportDates)

	api.StationByHashHandler = op.StationByHashHandlerFunc(svc.stationByHash)
	api.SaveIfNotExistsHandler = op.SaveIfNotExistsHandlerFunc(svc.saveIfNotExists)
	api.StationsVariablesHandler = op.StationsVariablesHandlerFunc(svc.StationsVariables)

	api.OpenStationHandler = op.OpenStationHandlerFunc(svc.openStation)

	api.SetProgramHandler = op.SetProgramHandlerFunc(svc.setProgram)
	api.ProgramsHandler = op.ProgramsHandlerFunc(svc.programs)
	api.SetStationButtonHandler = op.SetStationButtonHandlerFunc(svc.setStationButton)
	api.StationButtonHandler = op.StationButtonHandlerFunc(svc.stationButton)
	api.StationProgramByHashHandler = op.StationProgramByHashHandlerFunc(svc.stationProgramByHash)

	api.KasseHandler = op.KasseHandlerFunc(svc.kasse)
	api.SetKasseHandler = op.SetKasseHandlerFunc(svc.setKasse)

	api.GetUsersHandler = op.GetUsersHandlerFunc(svc.getUsers)
	api.GetUserHandler = op.GetUserHandlerFunc(svc.getUser)
	api.CreateUserHandler = op.CreateUserHandlerFunc(svc.createUser)
	api.UpdateUserHandler = op.UpdateUserHandlerFunc(svc.updateUser)
	api.DeleteUserHandler = op.DeleteUserHandlerFunc(svc.deleteUser)
	api.UpdateUserPasswordHandler = op.UpdateUserPasswordHandlerFunc(svc.updateUserPassword)

	api.CardReaderConfigHandler = op.CardReaderConfigHandlerFunc(svc.cardReaderConfig)
	api.SetCardReaderConfigHandler = op.SetCardReaderConfigHandlerFunc(svc.setCardReaderConfig)
	api.CardReaderConfigByHashHandler = op.CardReaderConfigByHashHandlerFunc(svc.cardReaderConfigByHash)

	api.SaveStationEventHandler = op.SaveStationEventHandlerFunc(svc.SaveStationEvent)
	api.StationEventsReportDatesHandler = op.StationEventsReportDatesHandlerFunc(svc.StationEventsReportDates)

	api.RunProgramHandler = op.RunProgramHandlerFunc(svc.runProgram)
	api.PressButtonHandler = op.PressButtonHandlerFunc(svc.pressButton)
	server := restapi.NewServer(api)
	server.Host = cfg.Host
	server.Port = cfg.Port
	server.CleanupTimeout = time.Second * time.Duration(cfg.CleanupTimeout)
	server.ReadTimeout = time.Second * time.Duration(cfg.ReadTimeout)
	server.WriteTimeout = time.Second * time.Duration(cfg.WriteTimeout)
	svc.unknownHash = map[string]time.Time{}
	err = svc.loadHash()
	if err != nil {
		return nil, err
	}
	log.Info("Swagger REST protocol", "version", swaggerSpec.Spec().Info.Version)
	return server, nil
}
