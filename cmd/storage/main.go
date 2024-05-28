package main

import (
	"context"
	"database/sql"
	"flag"
	"fmt"
	"os"
	"os/user"
	"path"
	"runtime"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit/entity/vo"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/rabbit"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/hal"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/auth"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/dal"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/extapi"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/flags"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/goose"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/memdb"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/migration"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/svckasse"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/svcweather"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/powerman/must"
	_ "github.com/powerman/narada4d/protocol/goose-postgres"
	"github.com/powerman/narada4d/schemaver"
	"github.com/powerman/pqx"
	"github.com/powerman/structlog"

	kaspi "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/kaspi-client"
	mngt "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/mngt-client"
	sbpclient "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client"
)

const (
	dbTimeoutMigrations     = 5 * time.Minute
	dbIdleTimeoutMigrations = 10 * time.Minute
	dbTimeoutService        = 5 * time.Minute
	dbIdleTimeoutService    = 10 * time.Minute

	connectTimeout  = 3 * time.Second // must be less than swarm's deploy.update_config.monitor
	dbTimeout       = 3 * time.Second
	dbIdleTimeout   = 10 * time.Second
	dbMaxOpenConns  = 30 // about ⅓ of server's max_connections
	dbParallelConns = 5  // a bit more than average
)

//nolint:gochecknoglobals
var (
	// set by ./build
	gitVersion    string
	gitBranch     string
	gitRevision   string
	buildDate     string
	useMemDB      bool
	checkSysTime  bool
	startDelaySec int

	cmd = strings.TrimSuffix(path.Base(os.Args[0]), ".test")
	ver = strings.Join(strings.Fields(strings.Join([]string{gitVersion, gitBranch, gitRevision, buildDate}, " ")), " ")
	log = structlog.New()
	cfg struct {
		version     bool
		logLevel    string
		db          pqx.Config
		goose       string
		gooseDir    string
		extapi      extapi.Config
		kasse       svckasse.Config
		rabbit      rabbit.Config
		sbp         sbpConfig
		storage     app.AppConfig
		hal         hal.Config
		testBoards  bool
		mngtConfig  mngt.RabbitConfig
		kaspiConfig kaspi.RabbitConfig
	}
)

// sbp
type sbpConfig struct {
	RabbitURL    string
	RabbitPort   string
	RabbitSecure bool

	EnvNameServerID       string
	EnvNameServerPassword string

	PaymentExpirationPeriod       time.Duration
	PaymentConfirmationPingPeriod time.Duration
}

// readSbpConfigFromFlag ...
func readSbpConfigFromFlag() {
	flag.StringVar(&cfg.sbp.RabbitURL, "sbp.RabbitURL", def.SbpRabbitHost, "sbp rabbit host for service connections")
	flag.StringVar(&cfg.sbp.RabbitPort, "sbp.RabbitPort", def.SbpRabbitPort, "sbp rabbit port for service connections")
	flag.BoolVar(&cfg.sbp.RabbitSecure, "sbp.RabbitSecure", def.SbpRabbitSecure, "sbp rabbit secure for service connections")

	flag.StringVar(&cfg.sbp.EnvNameServerID, "sbp.EnvNameServerID", def.SbpEnvNameServerID, "sbp env name for server_id")
	flag.StringVar(&cfg.sbp.EnvNameServerPassword, "sbp.EnvNameServerPassword", def.SbpEnvNameServerPassword, "sbp env name for server_password")

	flag.DurationVar(&cfg.sbp.PaymentExpirationPeriod, "sbp.PaymentExpirationPeriod", def.SbpPaymentExpirationPeriod, "sbp payment expiration period")
	flag.DurationVar(&cfg.sbp.PaymentConfirmationPingPeriod, "sbp.SbpPaymentConfirmationPingPeriod", def.SbpPaymentConfirmationPingPeriod, "sbp payment confirmation ping period")
}

// init provides common initialization for both app and tests.
func init() { //nolint:gochecknoinits
	err := def.Init()
	if err != nil {
		panic(err)
	}
	flag.BoolVar(&cfg.version, "version", false, "print version")
	flag.StringVar(&cfg.logLevel, "log.level", "debug", "log `level` (debug|info|warn|err)")
	flag.StringVar(&cfg.db.Host, "db.host", def.DBHost, "PostgreSQL `host`")
	flag.IntVar(&cfg.db.Port, "db.port", def.DBPort, "PostgreSQL `port`")
	flag.StringVar(&cfg.db.User, "db.user", def.DBUser, "PostgreSQL `user`")
	flag.StringVar(&cfg.db.Pass, "db.pass", def.DBPass, "PostgreSQL `pass`")
	flag.StringVar(&cfg.db.DBName, "db.name", def.DBName, "PostgreSQL `dbname`")
	flag.StringVar(&cfg.db.SearchPath, "db.schema", def.DBSchema, "PostgreSQL `search_path`")
	flag.StringVar(&cfg.goose, "goose", "", "run goose `COMMAND` and exit")
	flag.StringVar(&cfg.gooseDir, "goose.dir", def.GooseDir, "goose migrations `dir`")
	flag.StringVar(&cfg.extapi.Host, "extapi.host", def.ExtAPIHost, "serve external API on `host`")
	flag.IntVar(&cfg.extapi.Port, "extapi.port", def.ExtAPIPort, "serve external API on `port` (>0)")
	flag.StringVar(&cfg.extapi.BasePath, "extapi.basepath", def.ExtAPIBasePath, "serve external API on `path`")
	flag.StringVar(&cfg.hal.Endpoint, "hal.endpoint", def.HALEndpoint, "endpoint online kasse")
	flag.StringVar(&cfg.kasse.Endpoint, "kasse.endpoint", def.KasseEndpoint, "endpoint online kasse")
	flag.BoolVar(&useMemDB, "d", false, "change database from postgres to memdb")
	flag.BoolVar(&checkSysTime, "t", false, "check time")
	flag.BoolVar(&cfg.testBoards, "testboards", false, "test relay board")
	flag.IntVar(&cfg.extapi.CleanupTimeout, "extapi.cleanup-timeout", def.CleanupTimeout, "server timeout for cleaning in seconds")
	flag.IntVar(&cfg.extapi.ReadTimeout, "extapi.read-timeout", def.ReadTimeout, "server timeout for reading in seconds")
	flag.IntVar(&cfg.extapi.WriteTimeout, "extapi.write-timeout", def.WriteTimeout, "server timeout for writing in seconds")
	flag.IntVar(&startDelaySec, "delay", def.StartDelaySec, "startup delay in seconds")

	flag.StringVar(&cfg.rabbit.URL, "rabbit.host", def.RabbitHost, "host for service connections")
	flag.StringVar(&cfg.rabbit.Port, "rabbit.port", def.RabbitPort, "port for service connections")

	flag.StringVar(&cfg.storage.BonusServiceURL, "storage.bonus-service-url", def.OpenwashingURL, "URL of bonus service")

	// sbp
	readSbpConfigFromFlag()

	flag.StringVar(&cfg.mngtConfig.URL, "mngt.rabbitURL", def.MngtRabbitHost, "management rabbit host for service connections")
	flag.StringVar(&cfg.mngtConfig.Port, "mngt.RabbitPort", def.MngtRabbitPort, "management rabbit port for service connections")
	flag.BoolVar(&cfg.mngtConfig.Secure, "mngt.RabbitSecure", def.MngtRabbitSecure, "management rabbit secure for service connections")

	flag.StringVar(&cfg.kaspiConfig.URL, "kaspi.rabbitURL", def.KaspiRabbitHost, "kaspi rabbit host for service connections")
	flag.StringVar(&cfg.kaspiConfig.Port, "kaspi.RabbitPort", def.KaspiRabbitPort, "kaspi rabbit port for service connections")
	flag.BoolVar(&cfg.kaspiConfig.Secure, "kaspi.RabbitSecure", def.KaspiRabbitSecure, "kaspi rabbit secure for service connections")

	log.SetDefaultKeyvals(
		structlog.KeyUnit, "main",
	)
}

func main() { //nolint:gocyclo
	fmt.Printf("Getting ready to start ...\n")
	flag.Usage = func() {
		fmt.Printf("Usage of %s:\n", cmd)
		flag.PrintDefaults()
		fmt.Print(goose.Usage)
	}
	flag.Parse()
	log.Info(cfg.goose)
	if cfg.goose != "" {
		startDelaySec = 0
	}
	log.Info("main", "delay", startDelaySec)
	time.Sleep(time.Second * time.Duration(startDelaySec))

	if checkSysTime {
		waitUntilSysTimeIsCorrect()
	}

	switch {
	case cfg.db.Port <= 0:
		flags.FatalFlagValue("must be > 0", "db.port", cfg.db.Port)
	case !(cfg.goose == "" || goose.ValidCommand(cfg.goose)):
		flags.FatalFlagValue("", "goose", cfg.goose)
	case cfg.gooseDir == "":
		flags.FatalFlagValue("required", "goose.dir", cfg.gooseDir)
	case cfg.extapi.Port <= 0: // Dynamic port is not supported.
		flags.FatalFlagValue("must be > 0", "extapi.port", cfg.extapi.Port)
	case cfg.extapi.BasePath == "":
		flags.FatalFlagValue("required", "extapi.basepath", cfg.extapi.BasePath)
	case cfg.version: // Must be checked after all other flags for ease testing.
		fmt.Println(cmd, ver, runtime.Version())
		os.Exit(0)
	}

	if cfg.db.User == "" {
		u, err := user.Current()
		if err != nil {
			log.PrintErr(err)
		} else {
			cfg.db.User = u.Username
			cfg.db.Host = "/var/run/postgresql"
			cfg.db.Pass = ""
			cfg.db.Port = 0
		}
	}

	// Wrong log.level is not fatal, it will be reported and set to "debug".
	structlog.DefaultLogger.SetLogLevel(structlog.ParseLevel(cfg.logLevel))
	log.Info("started", "version", ver)

	var db *sqlx.DB
	var serviceDB *sqlx.DB
	var err error
	count := 0
	if !useMemDB {
		for db == nil {
			ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
			db, err = connectDB(ctx, dbTimeout, dbIdleTimeout)
			if err != nil {
				log.Warn("Warning: DB is not connected", "count", count, "err", err)
			}
			count++
			cancel()
		}
		count = 0
		for serviceDB == nil {
			ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
			serviceDB, err = connectDB(ctx, dbTimeoutService, dbIdleTimeoutService)
			if err != nil {
				log.Warn("Warning: DB for service actions is not connected", "count", count, "err", err)
			}
			count++
			cancel()
		}
	}
	errc := make(chan error)
	go run(db, serviceDB, errc)
	if err := <-errc; err != nil {
		log.Fatal(err)
	}
	log.Info("finished", "version", ver)
}

func connectDB(ctx context.Context, timeout time.Duration, idleTimeout time.Duration) (*sqlx.DB, error) {
	cfg.db.ConnectTimeout = connectTimeout
	cfg.db.SSLMode = pqx.SSLDisable
	cfg.db.DefaultTransactionIsolation = sql.LevelSerializable
	cfg.db.StatementTimeout = timeout
	cfg.db.LockTimeout = timeout
	cfg.db.IdleInTransactionSessionTimeout = idleTimeout

	db, err := sql.Open("pqx", cfg.db.FormatDSN())
	if err != nil {
		return nil, err
	}
	db.SetMaxOpenConns(dbMaxOpenConns)
	db.SetMaxIdleConns(dbParallelConns)

	err = db.PingContext(ctx)
	for err != nil {
		nextErr := db.PingContext(ctx)
		if nextErr == context.DeadlineExceeded {
			return nil, errors.Wrap(err, "connect to postgres")
		}
		err = nextErr
	}

	return sqlx.NewDb(db, "postgres"), nil
}

func applyMigrations(db *sqlx.DB) error {
	goose.Init("postgres")
	if db == nil {
		return errors.New("db not connected")
	}
	if cfg.goose != "" {
		err := goose.Run(db.DB, cfg.gooseDir, cfg.goose)
		if err != nil {
			return err
		}
		os.Exit(0)
		return nil
	}
	err := goose.UpTo(db.DB, cfg.gooseDir, migration.CurrentVersion)
	if err != nil {
		return err
	}

	must.NoErr(os.Setenv(schemaver.EnvLocation, "goose-"+cfg.db.FormatURL()))

	return nil

}

func run(db *sqlx.DB, maintenanceDBConn *sqlx.DB, errc chan<- error) {
	goose.Init("postgres")
	var repo app.Repo
	var sbpRepo app.SbpRepInterface
	if db != nil {
		var err error
		var dbMigrations *sqlx.DB

		count := 0

		for dbMigrations == nil {
			ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
			dbMigrations, err = connectDB(ctx, dbTimeoutMigrations, dbIdleTimeoutMigrations)
			if err != nil {
				log.Warn("Warning: DB is not connected", "count", count, "err", err)
			}
			count++
			cancel()
		}

		err = applyMigrations(dbMigrations)
		if err != nil {
			errc <- err
			return
		}
		dbMigrations.Close()

		repository := dal.New(db, maintenanceDBConn)
		repo = repository
		sbpRepo = repository
	} else {
		log.Info("USING MEM DB")
		repo = memdb.New()
	}

	kasse := svckasse.New(cfg.kasse)

	providerConfig := &svcweather.APIKeyConfig{}
	providerConfig.Name = app.MeteoInfo
	providerConfig.BaseURL = def.MeteoInfoBaseURL

	weather, errWeatherSvc := svcweather.Instance(providerConfig, nil)
	if errWeatherSvc != nil {
		log.Info("Weather IS TURNED OFF")
	}

	//hal client creation
	client, err := hal.NewClient(cfg.hal)
	if err != nil {
		errc <- err
		return
	}

	appl := app.New(repo, kasse, weather, client)
	go appl.PingServices()

	// bonus
	rabbitCfg, err := appl.GetRabbitConfig()
	if err == nil {
		cfg.rabbit.ServerID = rabbitCfg.ServerID
		cfg.rabbit.ServerKey = rabbitCfg.ServerKey

		go initRabbitClient(cfg.rabbit, appl)
	} else {
		log.Warn("no wash_bonus config found! skipping wash_bonus service initialization")
	}

	// init sbp
	initSbpClient(
		cfg.sbp.RabbitURL,
		cfg.sbp.RabbitPort,
		cfg.sbp.RabbitSecure,
		appl,
		sbpRepo,
		cfg.sbp.PaymentExpirationPeriod,
		cfg.sbp.PaymentConfirmationPingPeriod,
		cfg.sbp.EnvNameServerID,
		cfg.sbp.EnvNameServerPassword,
	)
	go initMngtClient(cfg.mngtConfig, appl)
	go initKaspiClient(cfg.kaspiConfig, appl)

	// server
	extsrv, err := extapi.NewServer(appl, cfg.extapi, repo, auth.NewAuthCheck(log, appl))
	if err != nil {
		errc <- err
		return
	}
	log.Info("serve Swagger REST protocol", def.LogHost, cfg.extapi.Host, def.LogPort, cfg.extapi.Port)
	errc <- extsrv.Serve()
}

func waitUntilSysTimeIsCorrect() {
	sysDate := (time.Now()).UTC()
	compDate := time.Date(2021, time.January, 1, 12, 0, 0, 0, time.UTC)
	for compDate.Sub(sysDate) > 0 {
		log.Info("System time is incorrect; app awaits correction")
		t1 := time.NewTimer(time.Minute)
		<-t1.C
		sysDate = (time.Now()).UTC()
	}
}

func initRabbitClient(cfg rabbit.Config, appl app.App) {
	errLoginCount := 0
	for {
		rabbitWorker, err := rabbit.NewClient(cfg, appl)
		if err != nil {
			if strings.Contains(err.Error(), "username or password not allowed") {
				log.Err("Failed to init rabbit client due to wrong credentials", "error", err)
				errLoginCount += 1
			}
			if err == app.ErrServiceNotConfigured {
				return
			}
			if errLoginCount > 10 {
				return
			}

			log.Err("Failed to init rabbit client", "error", err)
			time.Sleep(5 * time.Second)
			continue
		}

		log.Info("Serve rabbit client")
		appl.InitBonusRabbitWorker(string(vo.WashBonusService), rabbitWorker.SendMessage, rabbitWorker.Status)
		appl.FetchSessions()
		return
	}
}

func initMngtClient(cfg mngt.RabbitConfig, appl app.App) {
	errLoginCount := 0
	for {
		rabbitWorker, err := mngt.NewMngtRabbitClient(cfg, appl)
		if err != nil {
			if strings.Contains(err.Error(), "username or password not allowed") {
				log.Err("Failed to init management client due to wrong credentials", "error", err)
				errLoginCount += 1
			}
			if err == app.ErrServiceNotConfigured {
				return
			}
			if errLoginCount > 10 {
				return
			}

			log.Err("Failed to init management client", "error", err)
			time.Sleep(5 * time.Second)
			continue
		}

		log.Info("Serve management client")
		appl.InitManagement(rabbitWorker)
		return
	}
}

func initKaspiClient(cfg kaspi.RabbitConfig, appl app.App) {
	errLoginCount := 0
	for {
		rabbitWorker, err := kaspi.NewKaspiRabbitClient(cfg, appl)
		if err != nil {
			if strings.Contains(err.Error(), "username or password not allowed") {
				log.Err("Failed to init kaspi client due to wrong credentials", "error", err)
				errLoginCount += 1
			}
			if err == app.ErrServiceNotConfigured {
				return
			}

			if errLoginCount > 10 {
				return
			}

			log.Err("Failed to init kaspi client", "error", err)
			time.Sleep(5 * time.Second)
			continue
		}

		log.Info("Serve kaspi client")
		appl.InitKaspi(rabbitWorker)
		return
	}
}

// initSbpClient ...
func initSbpClient(
	url string,
	port string,
	secure bool,
	appl app.App,
	rep app.SbpRepInterface,
	sbpPaymentExpirationPeriod time.Duration,
	sbpPaymentConfirmationPingPeriod time.Duration,
	envServerSbpID string,
	envServerSbpPassword string,
) {
	sbpConfig, err := appl.GetSbpConfig(envServerSbpID, envServerSbpPassword)
	if err != nil {
		log.Warn("sbp configuration not found! skipping sbp service initialization")
	}
	if sbpConfig.ServerID == "" || sbpConfig.ServerPassword == "" {
		return
	}
	errLoginCount := 0
	go func() {
		for {
			// rabbit client
			sbpConfig := sbpclient.RabbitConfig{
				URL:            url,
				Port:           port,
				ServerID:       sbpConfig.ServerID,
				ServerPassword: sbpConfig.ServerPassword,
				Secure:         secure,
			}
			var sbpRabbitClient *sbpclient.Service
			sbpRabbitClient, err = sbpclient.NewSbpRabbitClient(sbpConfig, appl)
			if err != nil {
				if strings.Contains(err.Error(), "username or password not allowed") {
					log.Err("Failed to init sbp client due to wrong credentials", "error", err)
					errLoginCount += 1
				}
				if err == app.ErrServiceNotConfigured {
					return
				}
				if errLoginCount > 10 {
					return
				}

				log.Err("Failed to init sbp client", "error", err)
				time.Sleep(5 * time.Second)
				continue
			}

			// rabbit worker
			//isConnectedFunc := sbpRabbitWorker.IsConnected
			sbpWorkerConfig := app.SbpRabbitWorkerConfig{
				ServerID:                      sbpConfig.ServerID,
				ServerPassword:                sbpConfig.ServerPassword,
				SbpBroker:                     sbpRabbitClient,
				SbpRep:                        rep,
				NotificationExpirationPeriod:  sbpPaymentExpirationPeriod,
				PaymentConfirmationPingPeriod: sbpPaymentConfirmationPingPeriod,
			}

			err = appl.InitSbpRabbitWorker(sbpWorkerConfig)
			if err != nil {
				log.Err("Failed to init sbp sbp rabbit worker", "error", err)
			}

			log.Info("serve sbp rabbit client")
			return
		}
	}()
}
