package extapi

import (
	"net/http"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/auth"
	"github.com/OpenRbt/lea-central-wash/storageapi/restapi"
	"github.com/OpenRbt/lea-central-wash/storageapi/restapi/op"
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

	// ping
	api.PingHandler = op.PingHandlerFunc(svc.ping)
	api.GetPingHandler = op.GetPingHandlerFunc(svc.getPing)
	//

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

	api.RunProgramHandler = op.RunProgramHandlerFunc(svc.runProgram)
	api.Run2ProgramHandler = op.Run2ProgramHandlerFunc(svc.run2Program)
	api.MeasureVolumeMillilitersHandler = op.MeasureVolumeMillilitersHandlerFunc(svc.measureVolumeMilliliters)
	api.DispenserStopHandler = op.DispenserStopHandlerFunc(svc.dispenserStop)
	api.VolumeDispenserHandler = op.VolumeDispenserHandlerFunc(svc.VolumeDispenser)
	api.GetLevelHandler = op.GetLevelHandlerFunc(svc.GetLevel)
	api.PressButtonHandler = op.PressButtonHandlerFunc(svc.pressButton)

	api.StationStatCurrentHandler = op.StationStatCurrentHandlerFunc(svc.stationStatCurrent)
	api.StationStatDatesHandler = op.StationStatDatesHandlerFunc(svc.stationStatDates)
	api.ResetStationStatHandler = op.ResetStationStatHandlerFunc(svc.resetStationStat)

	api.AddAdvertisingCampaignHandler = op.AddAdvertisingCampaignHandlerFunc(svc.addAdvertisingCampaign)
	api.EditAdvertisingCampaignHandler = op.EditAdvertisingCampaignHandlerFunc(svc.editAdvertisingCampaign)
	api.AdvertisingCampaignHandler = op.AdvertisingCampaignHandlerFunc(svc.advertisingCampaign)
	api.AdvertisingCampaignByIDHandler = op.AdvertisingCampaignByIDHandlerFunc(svc.advertisingCampaignByID)
	api.DelAdvertisingCampaignHandler = op.DelAdvertisingCampaignHandlerFunc(svc.delAdvertistingCampagin)

	api.GetStationDiscountsHandler = op.GetStationDiscountsHandlerFunc(svc.getStationDiscount)

	api.SetConfigVarBoolHandler = op.SetConfigVarBoolHandlerFunc(svc.setConfigVarBool)
	api.SetConfigVarStringHandler = op.SetConfigVarStringHandlerFunc(svc.setConfigVarString)
	api.SetConfigVarIntHandler = op.SetConfigVarIntHandlerFunc(svc.setConfigVarInt)

	api.GetWashIDHandler = op.GetWashIDHandlerFunc(svc.getWashID)

	api.GetConfigVarBoolHandler = op.GetConfigVarBoolHandlerFunc(svc.getConfigVarBool)
	api.GetConfigVarStringHandler = op.GetConfigVarStringHandlerFunc(svc.getConfigVarString)
	api.GetConfigVarIntHandler = op.GetConfigVarIntHandlerFunc(svc.getConfigVarInt)

	api.SetStationConfigVarBoolHandler = op.SetStationConfigVarBoolHandlerFunc(svc.setStationConfigVarBool)
	api.SetStationConfigVarStringHandler = op.SetStationConfigVarStringHandlerFunc(svc.setStationConfigVarString)
	api.SetStationConfigVarIntHandler = op.SetStationConfigVarIntHandlerFunc(svc.setStationConfigVarInt)

	api.GetStationConfigVarBoolHandler = op.GetStationConfigVarBoolHandlerFunc(svc.getStationConfigVarBool)
	api.GetStationConfigVarStringHandler = op.GetStationConfigVarStringHandlerFunc(svc.getStationConfigVarString)
	api.GetStationConfigVarIntHandler = op.GetStationConfigVarIntHandlerFunc(svc.getStationConfigVarInt)

	api.GetStationWashConfigVarIntHandler = op.GetStationWashConfigVarIntHandlerFunc(svc.getStationWashConfigVarInt)
	api.GetStationWashConfigVarBoolHandler = op.GetStationWashConfigVarBoolHandlerFunc(svc.getStationWashConfigVarBool)
	api.GetStationWashConfigVarStringHandler = op.GetStationWashConfigVarStringHandlerFunc(svc.getStationWashConfigVarString)

	api.CreateSessionHandler = op.CreateSessionHandlerFunc(svc.createSession)
	api.EndSessionHandler = op.EndSessionHandlerFunc(svc.endSession)
	api.SetBonusesHandler = op.SetBonusesHandlerFunc(svc.setBonuses)
	api.GetServerInfoHandler = op.GetServerInfoHandlerFunc(svc.getServerInfo)

	api.GetPublicKeyHandler = op.GetPublicKeyHandlerFunc(svc.getPublicKey)
	api.GetStationFirmwareVersionsHandler = op.GetStationFirmwareVersionsHandlerFunc(svc.getFirmwareVersions)
	api.GetTaskHandler = op.GetTaskHandlerFunc(svc.getTask)
	api.GetListTasksHandler = op.GetListTasksHandlerFunc(svc.getListTasks)
	api.CreateTaskHandler = op.CreateTaskHandlerFunc(svc.createTask)
	api.CreateTaskByHashHandler = op.CreateTaskByHashHandlerFunc(svc.createTaskByHash)
	api.GetBuildScriptHandler = op.GetBuildScriptHandlerFunc(svc.getBuildScript)
	api.GetListBuildScriptsHandler = op.GetListBuildScriptsHandlerFunc(svc.getListBuildScripts)
	api.SetBuildScriptHandler = op.SetBuildScriptHandlerFunc(svc.setBuildScript)
	api.FirmwareVersionsCopyHandler = op.FirmwareVersionsCopyHandlerFunc(svc.copyFirmware)
	api.GetStationFirmwareVersionBufferedHandler = op.GetStationFirmwareVersionBufferedHandlerFunc(svc.getVersionBuffered)
	api.AddLogHandler = op.AddLogHandlerFunc(svc.addLog)

	// sbp
	api.PayHandler = op.PayHandlerFunc(svc.pay)
	api.PayReceivedHandler = op.PayReceivedHandlerFunc(svc.payReceived)

	// server
	server := restapi.NewServer(api)
	server.Host = cfg.Host
	server.Port = cfg.Port
	server.CleanupTimeout = time.Second * time.Duration(cfg.CleanupTimeout)
	server.ReadTimeout = time.Second * time.Duration(cfg.ReadTimeout)
	server.WriteTimeout = time.Second * time.Duration(cfg.WriteTimeout)

	globalMiddlewares := func(handler http.Handler) http.Handler {
		accesslog := makeAccessLog(cfg.BasePath)
		return recovery(accesslog(handler))
	}
	middlewares := func(handler http.Handler) http.Handler {
		return handler
	}

	server.SetHandler(globalMiddlewares(api.Serve(middlewares)))

	svc.unknownHash = map[string]time.Time{}
	err = svc.loadHash()
	if err != nil {
		return nil, err
	}
	log.Info("Swagger REST protocol", "version", swaggerSpec.Spec().Info.Version)
	return server, nil
}
