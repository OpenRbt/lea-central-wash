package app

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	uuid "github.com/satori/go.uuid"

	rabbit_vo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

const durationStationOffline = time.Second * 10

// For testing purposes
const (
	TestHash      = "TEST"
	TestStationID = 999
)

// Auth describes user profile.
type Auth = storageapi.Profile

const ParameterNameVolumeCoef = "VOLUME_COEF"

// Key aliases
const (
	TemperatureCurrent                      = "curr_temp"
	MeteoInfo                               = "meteoinfo"
	OpenWeather                             = "openWeather"
	Ipify                                   = "ipify"
	relayTimeoutSec                         = 5
	parameterNameTimeZone                   = "TIMEZONE"
	parameterNameLastMotorStatsUpdate       = "LAST_MOTOR_STATS_UPDATE"
	parameterNameLastMotorStatsByDateUpdate = "LAST_MOTOR_STATS_DATE_UPDATE"
)

const qrURL = "%s/#/?sessionID=%s"

const RabbitWorkerScheduleInterval = time.Minute

// Post control commands
const (
	openwashingName        = "openwashing"
	binarName              = "firmware.exe"
	paymentWorldName       = "uic_payment_app"
	paymentWorldConfigName = "config.ini"
	preparePiName          = "prepare_pi.sh"
	updateSystemName       = "update_system.sh"
	versionName            = "versions.json"
	currentWashName        = "current_wash"
	baseWashName           = "wash"
	runshName              = "run.sh"
	scriptLuaName          = "script.lua"
	mainJsonName           = "main.json"
	firmwareName           = "firmware"
	binarOwPath            = "openwashing/software/v1-enlight/firmware.exe"
	samplesPath            = "software/v1-enlight/samples"
	samplesName            = "samples"

	getFullPathLcwCommand     = "readlink -f %s"
	cdToTempLcwCommand        = "cd %s && %s"
	cloneRepositoryLcwCommand = "git clone https://github.com/OpenRbt/openwashing.git %s"
	pullRepositoryLcwCommand  = "cd %s && git pull"
	createLcwCommand          = "rm -rf %s && mkdir -p %s"
	hashLcwCommand            = "shasum %s | cut -d ' ' -f 1"
	hashEnvLcwCommand         = "find %s -type f -not -name 'script.lua' -exec shasum {} \\; | awk '{print $1}' | shasum | cut -d ' ' -f 1"
	commitedAtLcwCommand      = "cd %s && git log -1 --format='%%cd' --date=format:'%%Y-%%m-%%dT%%H:%%M:%%SZ'"
	cpLcwCommand              = "rm -rf %s && cp -r %s %s"

	cloneRepositoryOwCommand = "git clone https://github.com/OpenRbt/openwashing.git ~/openwashing && cd ~/openwashing/software/v1-enlight/3rd/lua53/src && make linux"
	pullRepositoryOwCommand  = "cd ~/openwashing && git pull"
	makeBinarOwCommand       = "cd ~/openwashing/software/v1-enlight && make"
	findVersionsOwCommand    = "find ~/ -maxdepth 1 -type d -name \"wash_*\" -o -name \"wash\""
	cleateLink               = "ln -f -s -n %s %s"
	rebootOwCommand          = "sudo shutdown -r +0"
	cdFirmwareRunshCommand   = "cd %s"
)

// Errors.
var (
	ErrNotFound                 = errors.New("not found")
	ErrNotFoundDispenser        = errors.New("not found Dispenser")
	ErrNotFoundBoard            = errors.New("not found Board")
	ErrAccessDenied             = errors.New("access denied")
	ErrPasswordNotUnique        = errors.New("password is already in use")
	ErrWrongPassword            = errors.New("wrong password")
	ErrLoginNotUnique           = errors.New("login is already in use")
	ErrRemovingOnlyAdmin        = errors.New("it is impossible to remove the only admin")
	ErrMoneyCollectionFkey      = errors.New("violates foreign key constraint on table money_collection")
	ErrUnknownProgram           = errors.New("unknown program")
	ErrUnknownStation           = errors.New("unknown station")
	ErrStationProgramMustUnique = errors.New("programID and buttonID must be unique")
	ErrUserIsNotAuthorized      = errors.New("user is not authorized")
	ErrSessionNotFound          = errors.New("session not found")
	ErrWrongParameter           = errors.New("wrong parameter")
	ErrTaskStarted              = errors.New("task started")
	ErrStationDirectoryNotExist = errors.New("station directory does not exist")

	ErrServiceNotConfigured    = errors.New("service not configured")
	ErrRabbitMessageBadPayload = errors.New("bad RabbitMessagePayloadData")
	ErrNoRabbitWorker          = errors.New("rabbit worker not initialized")
	ErrSendTimeout             = errors.New("send request failed: timeout")
	ErrNotConfirmed            = errors.New("send request failed: not confirmed")

	ErrWrongPaymentStatus = errors.New("wrong payment status")
	ErrSameOrLowerVersion = errors.New("entity has the same or lower version")
)

var testApp = false

type (
	// App is an application interface.
	App interface {
		// Key-value methods
		Save(stationID StationID, key string, value string) error
		SaveIfNotExists(stationID StationID, key string, value string) error
		Load(stationID StationID, key string) (string, error)
		StationsVariables() ([]StationsVariables, error)
		FetchSessions() error

		// DBMS info method
		Info() string

		AddServiceAmount(stationID StationID, money int) error
		AddServiceAmountForManagement(ctx context.Context, id StationID, money int) error
		OpenStation(StationID) error

		Set(station StationData) error
		Get(stationID StationID) (StationData, error)
		Ping(id StationID, balance, program int, stationIP string) (StationData, bool)

		SaveMoneyReport(report MoneyReport) error
		SaveRelayReport(report RelayReport) error
		LoadMoneyReport(StationID) (*MoneyReport, error)
		RelayReportCurrent(auth *Auth, id *StationID) (StationsStat, error)

		StatusReport(bool) StatusReport
		SetStation(ctx context.Context, station SetStation) error
		DelStation(ctx context.Context, id StationID) error
		StationReportDates(id StationID, startDate, endDate time.Time) (MoneyReport, RelayReport, error)
		StationReportCurrentMoney(id StationID) (MoneyReport, RelayReport, error)
		CollectionReports(id StationID, startDate, endDate *time.Time) (reports []CollectionReportWithUser, err error)

		StatusCollection() StatusCollection
		SaveCollectionReport(auth *Auth, id StationID) error

		Programs(id *int64) ([]Program, error)
		GetProgramsForManagement(ctx context.Context, filter ProgramFilter) (Page[Program], error)
		SetProgram(Program) error
		SetProgramFromManagement(ctx context.Context, program Program) (Program, error)
		NotSendedPrograms(ctx context.Context) ([]Program, error)
		MarkProgramSended(ctx context.Context, id int64) error

		StationProgram(StationID) ([]StationProgram, error)
		SetStationProgram(context.Context, StationID, []StationProgram) error
		StationConfig(StationID) (StationConfig, error)

		Users(ctx context.Context, auth *Auth) (users []User, err error)
		User(ctx context.Context, password string) (user User, err error)
		CreateUser(ctx context.Context, userCreation UserCreation, auth *Auth) (user User, err error)
		UpdateUser(login string, userUpdate UserUpdate, auth *Auth) (user User, err error)
		UpdateUserPassword(ctx context.Context, login string, userData UpdatePasswordData, auth *Auth) (user User, err error)
		DeleteUser(ctx context.Context, login string, auth *Auth) error
		GetUsersForManagement(ctx context.Context, filter UserFilter) (Page[User], error)
		GetUserForManagement(ctx context.Context, login string) (User, error)
		CreateUsersForManagement(ctx context.Context, userCreation UserCreation) (User, error)
		UpdateUsersForManagement(ctx context.Context, login string, userUpdate UserUpdate) (User, error)
		DeleteUsersForManagement(ctx context.Context, login string) (User, error)
		ChangeUserPasswordForManagement(ctx context.Context, login string, password UpdatePasswordData) (User, error)

		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(context.Context, CardReaderConfig) error

		RunProgram(id StationID, programID int64, preflight bool) (err error)
		Run2Program(id StationID, programID int64, programID2 int64, preflight bool) (err error)
		MeasureVolumeMilliliters(volume int64, stationID StationID, startProgramID int64, stopProgramID int64) (err error)
		GetVolumeDispenser() (volume int64, status string, err error)
		DispenserStop(stationID StationID, stopProgramID int64) (err error)
		GetLevel() (level int64, err error)
		PressButton(id StationID, buttonID int64) (err error)

		Station(StationID) (SetStation, error)
		RelayReportDates(auth *Auth, stationID *StationID, startDate, endDate time.Time) (StationsStat, error)
		ResetStationStat(auth *Auth, stationID StationID) error

		AddAdvertisingCampaign(context.Context, *Auth, AdvertisingCampaign) (AdvertisingCampaign, error)
		EditAdvertisingCampaign(auth *Auth, a AdvertisingCampaign) error
		DelAdvertisingCampaign(auth *Auth, id int64) (err error)
		AdvertisingCampaignByID(auth *Auth, id int64) (AdvertisingCampaign, error)
		AdvertisingCampaign(auth *Auth, startDate, endDate *time.Time) ([]AdvertisingCampaign, error)

		GetAdvertisingCampaignsForManagement(ctx context.Context, filter AdvertisingCampaignFilter) (Page[AdvertisingCampaign], error)
		GetAdvertisingCampaignByIDForManagement(ctx context.Context, id int64) (AdvertisingCampaign, error)
		AddAdvertisingCampaignFromManagement(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error)
		EditAdvertisingCampaignFromManagement(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error)
		DeleteAdvertisingCampaignFromManagement(ctx context.Context, id int64) (AdvertisingCampaign, error)

		UpsertAdvertisingCampaignFromManagement(ctx context.Context, campaign ManagementAdvertisingCampaign) (AdvertisingCampaign, error)
		NotSendedAdvertisingCampaigns(ctx context.Context) ([]AdvertisingCampaign, error)
		MarkAdvertisingCampaignSended(ctx context.Context, id int64) error

		GetStationDiscount(id StationID) (*StationDiscount, error)

		GetConfigInt(auth *Auth, name string) (ConfigInt, error)
		GetConfigBool(auth *Auth, name string) (ConfigBool, error)
		GetConfigString(auth *Auth, name string) (ConfigString, error)

		SetConfigInt(auth *Auth, config ConfigInt) error
		SetConfigBool(auth *Auth, config ConfigBool) error
		SetConfigString(auth *Auth, config ConfigString) error

		DeleteConfigString(auth *Auth, name string) error

		GetStationConfigInt(name string, stationID StationID) (StationConfigVar[int64], error)
		GetStationConfigBool(name string, stationID StationID) (StationConfigVar[bool], error)
		GetStationConfigString(name string, stationID StationID) (StationConfigVar[string], error)

		SetStationConfigInt(auth *Auth, config StationConfigVar[int64]) error
		SetStationConfigBool(auth *Auth, config StationConfigVar[bool]) error
		SetStationConfigString(auth *Auth, config StationConfigVar[string]) error

		CreateSession(url string, stationID StationID) (string, string, error)
		EndSession(stationID StationID, sessionID BonusSessionID) error
		SetBonuses(stationID StationID, bonuses int) error

		GetRabbitConfig() (RabbitConfig, error)
		GetServerInfo() ServerInfo
		SetExternalServicesActive(active bool)
		SetNextSession(stationID StationID) error
		RequestSessionsFromService(count int, stationID StationID) error
		AddSessionsToPool(stationID StationID, sessionsIDs ...string) error
		AssignSessionUser(sessionID string, userID string, post StationID) error
		AssignSessionBonuses(sessionID string, amount int, post StationID) error

		InitBonusRabbitWorker(routingKey string, publisherFunc func(msg interface{}, service rabbit_vo.Service, target rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) error, status func() ServiceStatus)

		// sbp
		SendPaymentRequest(postID StationID, amount int64) error
		// set
		SetPaymentURL(orderID uuid.UUID, urlPay string) error
		ReceiveNotification(orderID uuid.UUID, status PaymentStatus) error
		SetPaymentReceived(orderID uuid.UUID) error
		SetPaymentCanceled(orderID uuid.UUID) (err error)
		// get
		GetLastPayment(postID StationID) (Payment, error)
		InitSbpRabbitWorker(config SbpRabbitWorkerConfig) error
		IsSbpRabbitWorkerInit() bool
		IsSbpAvailableForStation(stationID StationID) bool
		GetSbpConfig(envServerSbpID string, envServerSbpPassword string) (cfg SbpRabbitConfig, err error)

		InitManagement(ManagementRabbitWorker)
		InitKaspi(KaspiService)
		KaspiCommand(Command)

		PingServices()
		GetPublicKey() (string, error)
		GetVersions(stationID StationID) ([]FirmwareVersion, error)
		GetListTasks(filter TaskFilter) (Page[Task], error)
		GetTask(id int) (Task, error)
		CreateTask(createTask CreateTask) (Task, error)
		GetTasksForManagement(ctx context.Context, filter TaskFilter) (Page[Task], error)
		GetTaskByIdForManagement(ctx context.Context, id int) (Task, error)
		CreateTaskForManagement(ctx context.Context, createTask CreateTask) (Task, error)
		CopyFirmwareForManagement(ctx context.Context, stationID StationID, copyToID StationID) error
		GetVersionBufferedForManagement(ctx context.Context, stationID StationID) (FirmwareVersion, error)
		GetVersionsForManagement(ctx context.Context, stationID StationID) ([]FirmwareVersion, error)
		GetListBuildScripts() ([]BuildScript, error)
		GetBuildScript(id StationID) (BuildScript, error)
		SetBuildScript(setBuildScript SetBuildScript) (BuildScript, error)
		DeleteBuildScript(id StationID) error
		CopyFirmware(stationID StationID, copyToID StationID) error
		GetVersionBuffered(stationID StationID) (FirmwareVersion, error)

		StationUpdateForManagement(ctx context.Context, id StationID, station StationUpdate) (StationConfig, error)
		StationGetForManagement(ctx context.Context, id StationID) (StationConfig, error)

		AddOpenwashingLog(log OpenwashingLogCreate) (OpenwashingLog, error)
	}

	// Repo is a DAL interface.
	Repo interface {
		Save(stationID StationID, key string, value string) error
		SaveIfNotExists(stationID StationID, key string, value string) error
		Load(stationID StationID, key string) (string, error)
		Info() string
		SetStation(station SetStation) error
		Stations() (stations []SetStation, err error)
		DelStation(StationID) error
		LastMoneyReport(stationID StationID) (MoneyReport, error)
		SaveMoneyReport(MoneyReport) error
		RelayReportCurrent(stationID *StationID) (StationsStat, error)
		SaveRelayReport(RelayReport) error
		MoneyReport(stationID StationID, startDate, endDate time.Time) (MoneyReport, error)
		RelayStatReport(stationID StationID, startDate, endDate time.Time) (RelayReport, error)
		LastCollectionReport(stationID StationID) (report CollectionReport, err error)
		SaveCollectionReport(userID int, stationID StationID) (err error)
		CollectionReports(id StationID, startDate, endDate *time.Time) (reports []CollectionReportWithUser, err error)
		StationsVariables() ([]StationsVariables, error)
		AddStation(name string) error
		AddOpenStationLog(StationID) error
		CurrentMoney(StationID) (MoneyReport, error)

		User(login string) (user User, err error)
		Users(ctx context.Context, filter UserFilter) (users []User, count int64, err error)
		CreateUser(userData UserCreation) (newUser User, err error)
		UpdateUser(login string, userData UserUpdate) (newUser User, err error)
		DeleteUser(ctx context.Context, login string) (User, error)

		// for api
		LoadHash() ([]StationID, []string, error)
		SetHash(StationID, string) error
		CheckDB() (ok bool, err error)

		GetPrograms(ctx context.Context, filter ProgramFilter) ([]Program, int64, error)
		SetProgram(ctx context.Context, program Program) (Program, error)
		SetProgramFromManagement(ctx context.Context, program ManagementProgram) (Program, error)
		NotSendedPrograms(ctx context.Context) ([]Program, error)
		MarkProgramSended(ctx context.Context, id int64) error

		StationProgram(StationID) ([]StationProgram, error)
		SetStationProgram(StationID, []StationProgram) error
		StationConfig(StationID) (StationConfig, error)
		Station(StationID) (SetStation, error)
		StationUpdate(context.Context, StationID, StationUpdate) (StationConfig, error)

		Kasse() (kasse Kasse, err error)
		SetKasse(kasse Kasse) (err error)
		CardReaderConfig(StationID) (*CardReaderConfig, error)
		SetCardReaderConfig(CardReaderConfig) error
		AddUpdateConfig(note string) (int, error)
		LastUpdateConfig() (int, error)
		RelayReportDates(stationID *StationID, startDate, endDate time.Time) (StationsStat, error)
		ResetStationStat(stationID StationID) error

		AddAdvertisingCampaign(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error)
		EditAdvertisingCampaign(ctx context.Context, campaign AdvertisingCampaign) (AdvertisingCampaign, error)
		DeleteAdvertisingCampaign(ctx context.Context, id int64) (AdvertisingCampaign, error)
		GetAdvertisingCampaignByID(ctx context.Context, id int64) (AdvertisingCampaign, error)
		GetAdvertisingCampaigns(ctx context.Context, filter AdvertisingCampaignFilter) ([]AdvertisingCampaign, int64, error)

		UpsertAdvertisingCampaignFromManagement(ctx context.Context, advert ManagementAdvertisingCampaign) (AdvertisingCampaign, error)
		NotSendedAdvertisingCampaigns(ctx context.Context) ([]AdvertisingCampaign, error)
		MarkAdvertisingCampaignSended(ctx context.Context, id int64) error

		NotSendedConfigStrings(ctx context.Context) ([]ConfigString, error)
		MarkConfigStringSended(ctx context.Context, name string) error
		NotSendedConfigInts(ctx context.Context) ([]ConfigInt, error)
		MarkConfigIntSended(ctx context.Context, name string) error
		NotSendedConfigBools(ctx context.Context) ([]ConfigBool, error)
		MarkConfigBoolSended(ctx context.Context, name string) error
		NotSendedStationConfigStrings(ctx context.Context) ([]StationConfigVar[string], error)
		MarkStationConfigStringSended(ctx context.Context, name string, stationID StationID) error
		NotSendedStationConfigBools(ctx context.Context) ([]StationConfigVar[bool], error)
		MarkStationConfigBoolSended(ctx context.Context, name string, stationID StationID) error
		NotSendedStationConfigInts(ctx context.Context) ([]StationConfigVar[int64], error)
		MarkStationConfigIntSended(ctx context.Context, name string, stationID StationID) error
		NotSendedUsers(ctx context.Context) ([]User, error)
		MarkUserSended(ctx context.Context, login string) error

		GetCurrentAdvertisingCampaigns(time.Time) ([]AdvertisingCampaign, error)

		GetConfigInt(name string) (ConfigInt, error)
		GetConfigBool(name string) (ConfigBool, error)
		GetConfigString(name string) (ConfigString, error)

		SetConfigInt(config ConfigInt) error
		SetConfigBool(config ConfigBool) error
		SetConfigString(config ConfigString) error

		DeleteConfigString(name string) error

		GetStationConfigInt(name string, stationID StationID) (StationConfigVar[int64], error)
		GetStationConfigBool(name string, stationID StationID) (StationConfigVar[bool], error)
		GetStationConfigString(name string, stationID StationID) (StationConfigVar[string], error)

		SetStationConfigInt(config StationConfigVar[int64]) error
		SetStationConfigBool(config StationConfigVar[bool]) error
		SetStationConfigString(config StationConfigVar[string]) error

		SetConfigIntIfNotExists(ConfigInt) error

		SaveMoneyReportAndMessage(report RabbitMoneyReport) (err error)
		AddRabbitMessage(message RabbitMessage) error
		GetUnsendedRabbitMessages(lastMessageID int64) ([]RabbitMessage, error)
		GetUnsendedMoneyReports(lastMessageID int64) (rabbitMoneyReports []RabbitMoneyReport, err error)
		MarkRabbitMoneyReportAsSent(id int64) (err error)
		MarkRabbitMessageAsSent(id int64) (err error)

		RefreshMotorStatsCurrent() (err error)
		RefreshMotorStatsDates() (err error)
		Collections() ([]CollectionReport, error)
		CollectionSetSended(int) error
		MoneyReports() ([]MngtMoneyReport, error)
		MoneyReportSetSended(int) error

		GetListBuildScripts() ([]BuildScript, error)
		GetBuildScript(id int) (BuildScript, error)
		GetBuildScriptByStationID(id StationID) (BuildScript, error)
		CreateBuildScript(createBuildScript SetBuildScript) (BuildScript, error)
		UpdateBuildScript(id int, updateBuildScript SetBuildScript) (BuildScript, error)
		DeleteBuildScript(id int) error
		DeleteBuildScriptByStationID(id StationID) error
		GetListTasks(filter TaskFilter) ([]Task, int64, error)
		GetTask(id int) (Task, error)
		CreateTask(createTask CreateTask) (Task, error)
		UpdateTask(id int, updateTask UpdateTask) (Task, error)
		NotSendedTasks(ctx context.Context) ([]Task, error)
		MarkTaskSended(ctx context.Context, id int) error
		NotSendedStations(ctx context.Context) ([]StationConfig, error)
		MarkStationSended(ctx context.Context, id StationID) error
		StationUpVersion(ctx context.Context, id StationID) error

		CreateOpenwashingLog(model OpenwashingLogCreate) (OpenwashingLog, error)
		NotSendedOpenwashingLogs(ctx context.Context) ([]OpenwashingLog, error)
		MarkOpenwashingLogSended(ctx context.Context, id int64) error
	}
	// KasseSvc is an interface for kasse service.
	KasseSvc interface {
		Info() (string, error)
	}
	// WeatherSvc is an interface for the weather service
	WeatherSvc interface {
		CurrentTemperature() (float64, error)
	}
	// HardwareAccessLayer describes an interface to access hardware control modules
	HardwareAccessLayer interface {
		RunProgram(id int32, cfg RelayConfig) (err error)
		MeasureVolumeMilliliters(volume int64, stationID StationID, startCfg RelayConfig, stopCfg RelayConfig) (err error)
		DispenserStop(stationID StationID, cfg RelayConfig) (err error)
		Volume() (volume int64, status string, err error)
		GetLevel() (level int64, err error)
	}
	// ControlBoard represents one board (even virtual) to control relays
	ControlBoard interface {
		StopAll() error
		MyPosition() (int, error)
		RunConfig(config RelayConfig)
	}
	// RelayConfig represents a relay config for something
	RelayConfig struct {
		// MotorSpeedPercent  will be passed to ESQ500/600 or another frequency controller to change the motor speed
		// NOT MORE THAN 150 PERCENT!!!
		MotorSpeedPercent int
		// If anything happens with the whole system, the control board will stop all after this time
		// NOT MORE THAN 3600 SECONDS!!!
		TimeoutSec int
		// Timings are settings for actual relays
		Timings []Relay
	}
	ManagementRabbitWorker interface {
		SendMoneyReport(MngtMoneyReport) error
		SendCollectionReport(CollectionReport) error
		Status() ServiceStatus
		SendStatus(StatusReport, bool) error
		SendProgram(Program) error
		SendOpenwashingLog(OpenwashingLog) error
		SendAdvertisingCampaign(AdvertisingCampaign) error
		SendConfigString(ConfigString) error
		SendConfigInt(ConfigInt) error
		SendConfigBool(ConfigBool) error
		SendStationConfigBool(StationConfigVar[bool]) error
		SendStationConfigString(StationConfigVar[string]) error
		SendStationConfigInt(StationConfigVar[int64]) error
		SendUser(User) error
		SendTask(Task) error
		SendStation(StationConfig) error
	}
	management struct {
		syncChannel chan struct{}
		ManagementRabbitWorker
	}
	KaspiService interface {
		Status() ServiceStatus
		SendAnswer(KaspiAnswer) error
		Ping(serverID string, status []StationPingStatus) error
	}
)

type app struct {
	repo          Repo
	stations      map[StationID]StationData
	stationsMutex sync.RWMutex

	stationsSessionsPool    map[StationID]chan string
	stationSessionPoolMutex sync.RWMutex

	programs      map[int64]Program
	programsMutex sync.RWMutex

	programsDiscounts     ProgramsDiscount
	programsDiscountMutex sync.Mutex
	kasseSvc              KasseSvc
	weatherSvc            WeatherSvc
	hardware              HardwareAccessLayer
	lastUpdate            int
	lastDiscountUpdate    int64
	cfg                   AppConfig
	cfgMutex              sync.Mutex
	volumeCorrection      int
	mngtSvc               management
	kaspiSvc              KaspiService

	postControlConfig PostControlConfig

	extServicesActive     bool
	servicesPublisherFunc func(msg interface{}, service rabbit_vo.Service, target rabbit_vo.RoutingKey, messageType rabbit_vo.MessageType) error

	bonusSystemRabbitWorker *BonusRabbitWorker
	// sbp
	*SbpWorker
}

// New creates and returns new App.
func New(repo Repo, kasseSvc KasseSvc, weatherSvc WeatherSvc, hardware HardwareAccessLayer, postControlConfig PostControlConfig) App {
	appl := &app{
		repo:              repo,
		stations:          make(map[StationID]StationData),
		kasseSvc:          kasseSvc,
		weatherSvc:        weatherSvc,
		hardware:          hardware,
		volumeCorrection:  1000,
		postControlConfig: postControlConfig,
	}

	stationConfig, err := appl.repo.GetStationConfigInt(ParameterNameVolumeCoef, StationID(1))
	if err != nil {
		log.PrintErr(err)
	}
	appl.stationsMutex.Lock()
	appl.volumeCorrection = int(stationConfig.Value)
	log.Info("New App", "VOLUME_COEF", appl.volumeCorrection)
	appl.stationsMutex.Unlock()
	err = appl.setDefaultConfig()
	if err != nil {
		log.PrintErr(err)
	}
	err = appl.loadConfig()
	if err != nil {
		log.PrintErr(err)
	}
	appl.loadStations()
	appl.loadPrograms()
	id, err := appl.repo.LastUpdateConfig()
	if err != nil {
		log.PrintErr(err)
		id = 1
	}
	appl.stationsMutex.Lock()
	appl.lastUpdate = id
	appl.stationsMutex.Unlock()
	go appl.runCheckStationOnline()
	go appl.refreshDiscounts()
	go appl.refreshMotorStatsCurrent()
	go appl.refreshMotorStatsDates()
	go appl.taskScheduler()
	return appl
}

func Ptr[T any](value T) *T {
	return &value
}

// Status describes station or kasse status.
type Status int

// StationID car wash station number
type StationID int

func (s StationID) String() string {
	return fmt.Sprintf("%d", s)
}

// BonusSessionID external bonus session uuid
type BonusSessionID string

// Status.
const (
	StatusOffline Status = 1
	StatusOnline  Status = 2
)

// StatusCollection is a report about how much money were in a station
type StatusCollection struct {
	Stations []CollectionReport
}

// StatusReport is just a status information
type StatusReport struct {
	KasseInfo   string
	KasseStatus Status
	LCWInfo     string
	Stations    []StationStatus
	BonusStatus ServiceStatus
	SbpStatus   ServiceStatus
	MngtStatus  ServiceStatus
}

// StationStatus is used to display in the managment software
type StationStatus struct {
	ID             StationID
	Info           string
	Name           string
	Status         Status
	CurrentBalance int
	CurrentProgram int
	ProgramName    string
	IP             string
	Version        *FirmwareVersion
}

type StationPingStatus struct {
	ID       StationID
	IsOnline bool
}

// SetStation is a struct to assign a name
type SetStation struct {
	ID           StationID
	Name         string
	PreflightSec int
	RelayBoard   string
	IsActive     bool
}

// StationsVariables represents a named variable for a specific Station
type StationsVariables struct {
	ID      StationID
	Name    string
	KeyPair []KeyPair
}

// KeyPair is just a Key and its value
type KeyPair struct {
	Key   string
	Value string
}

// Relay is a config for a relay
type Relay struct {
	ID      int
	TimeOn  int
	TimeOff int
}

// Program represents a program like Wax or Water or whatever ...
type Program struct {
	ID                         int64
	ButtonID                   int
	Price                      int
	Name                       string
	PreflightEnabled           bool
	MotorSpeedPercent          int64
	PreflightMotorSpeedPercent int64
	IsFinishingProgram         bool
	Relays                     []Relay
	PreflightRelays            []Relay
	Version                    int
}

type StationProgram struct {
	ButtonID  int
	ProgramID int
}

// Kasse is about connected Kasse device
type Kasse struct {
	ReceiptItem     string
	TaxType         string
	CashierFullName string
	CashierINN      string
}

type StationConfig struct {
	ID           StationID
	Name         string
	PreflightSec int
	RelayBoard   string
	LastUpdate   int
	Programs     []Program
	CardReader   CardReaderConfig
	Version      int
	Deleted      bool
}

type StationUpdate struct {
	Name         *string
	PreflightSec *int
	RelayBoard   *string
	Buttons      []StationProgram
	CardReader   *CardReaderConfig
}

type SessionsRequest struct {
	Count  int `json:"new_sessions_amount"`
	PostID int `json:"post_id"`
}

type RabbitMessageID int64

type RabbitMessage struct {
	ID          RabbitMessageID
	MessageType string
	Payload     interface{}
	CreatedAt   time.Time
	IsSent      bool
	SentAt      *time.Time
}

type RabbitMoneyReport struct {
	ID          RabbitMessageID
	MessageType string
	MoneyReport MoneyReport
	MessageUUID uuid.UUID
	CreatedAt   time.Time
	IsSent      bool
	SentAt      *time.Time
}

type ServerInfo struct {
	BonusServiceURL string `json:"bonusServiceURL,omitempty"`
}

type RabbitMessageType string

func (m RabbitMessageType) String() string {
	return string(m)
}

type RabbitRoutingKey string

func (r RabbitRoutingKey) String() string {
	return string(r)
}

type PostControlConfig struct {
	KeySSHPath      string
	UserSSH         string
	StationsDirPath string
}

type Page[T any] struct {
	Items      []T
	Page       int64
	PageSize   int64
	TotalPages int64
	TotalCount int64
}

type Pagination struct {
	Page     int64
	PageSize int64
}

func NewPage[T any](items []T, filter Pagination, totalItems int64) Page[T] {
	var totalPages int64
	if totalItems > 0 {
		totalPages = (totalItems-1)/filter.PageSize + 1
	}

	return Page[T]{
		Items:      items,
		TotalPages: totalPages,
		Page:       filter.Page,
		PageSize:   filter.PageSize,
		TotalCount: totalItems,
	}
}

func (f *Pagination) Offset() int64 {
	page := f.Page
	pageSize := f.PageSize

	if f.PageSize < 0 {
		pageSize = 0
	}

	if f.Page < 1 {
		page = 1
	}

	return (page - 1) * pageSize
}

func (f *Pagination) Limit() int64 {
	if f.PageSize < 0 {
		return 0
	}

	return f.PageSize
}
