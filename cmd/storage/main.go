package main

import (
	"context"
	"database/sql"
	"flag"
	"fmt"
	"github.com/OpenRbt/share_business/wash_rabbit/entity/vo"
	"os"
	"os/user"
	"path"
	"runtime"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/rabbit"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/hal"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/auth"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/dal"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/def"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/extapi"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/flags"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/goose"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/memdb"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/migration"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/svckasse"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/svcweather"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/powerman/must"
	_ "github.com/powerman/narada4d/protocol/goose-postgres"
	"github.com/powerman/narada4d/schemaver"
	"github.com/powerman/pqx"
	"github.com/powerman/structlog"
)

const (
	dbTimeoutMigrations     = 5 * time.Minute
	dbIdleTimeoutMigrations = 10 * time.Minute
	dbServiceTimeout        = 5 * time.Minute
	dbServiceIdleTimeout    = 10 * time.Minute

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

	RabbitCertPath string

	cmd = strings.TrimSuffix(path.Base(os.Args[0]), ".test")
	ver = strings.Join(strings.Fields(strings.Join([]string{gitVersion, gitBranch, gitRevision, buildDate}, " ")), " ")
	log = structlog.New()
	cfg struct {
		version    bool
		logLevel   string
		db         pqx.Config
		goose      string
		gooseDir   string
		extapi     extapi.Config
		kasse      svckasse.Config
		rabbit     rabbit.Config
		hal        hal.Config
		testBoards bool
	}
)

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

	flag.StringVar(&cfg.rabbit.Url, "rabbit.host", def.RabbitHost, "host for service connections")
	flag.StringVar(&cfg.rabbit.Port, "rabbit.port", def.RabbitPort, "port for service connections")

	flag.StringVar(&RabbitCertPath, "pathCert", def.RabbitCertPath, "path to cert Rabbit")

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
			db, err = connectDB(ctx)
			if err != nil {
				log.Warn("Warning: DB is not connected", "count", count, "err", err)
			}
			count++
			cancel()
		}
		count = 0
		for serviceDB == nil {
			ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
			serviceDB, err = connectDBMaintenance(ctx)
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

func connectDB(ctx context.Context) (*sqlx.DB, error) {
	cfg.db.ConnectTimeout = connectTimeout
	cfg.db.SSLMode = pqx.SSLDisable
	cfg.db.DefaultTransactionIsolation = sql.LevelSerializable
	cfg.db.StatementTimeout = dbTimeout
	cfg.db.LockTimeout = dbTimeout
	cfg.db.IdleInTransactionSessionTimeout = dbIdleTimeout

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

func connectDBMigrations(ctx context.Context) (*sqlx.DB, error) {
	cfg.db.ConnectTimeout = connectTimeout
	cfg.db.SSLMode = pqx.SSLDisable
	cfg.db.DefaultTransactionIsolation = sql.LevelSerializable
	cfg.db.StatementTimeout = dbTimeoutMigrations
	cfg.db.LockTimeout = dbTimeoutMigrations
	cfg.db.IdleInTransactionSessionTimeout = dbIdleTimeoutMigrations

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

func connectDBMaintenance(ctx context.Context) (*sqlx.DB, error) {
	cfg.db.ConnectTimeout = connectTimeout
	cfg.db.SSLMode = pqx.SSLDisable
	cfg.db.DefaultTransactionIsolation = sql.LevelSerializable
	cfg.db.StatementTimeout = dbServiceTimeout
	cfg.db.LockTimeout = dbServiceTimeout
	cfg.db.IdleInTransactionSessionTimeout = dbServiceIdleTimeout

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
		return goose.Run(db.DB, cfg.gooseDir, cfg.goose)
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
	if db != nil {
		var err error
		var dbMigrations *sqlx.DB

		count := 0

		for dbMigrations == nil {
			ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
			dbMigrations, err = connectDBMigrations(ctx)
			if err != nil {
				log.Warn("Warning: DB is not connected", "count", count, "err", err)
			}
			count++
			cancel()
		}
		defer dbMigrations.Close()

		err = applyMigrations(dbMigrations)
		if err != nil {
			errc <- err
			return
		}

		schemaVer, _ := schemaver.New()
		repo = dal.New(db, maintenanceDBConn, schemaVer)
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

	rabbitCfg, err := appl.GetRabbitConfig()
	if err != nil {
		log.Warn("no wash_bonus config found! skipping wash_bonus service initialization")
	} else {
		cfg.rabbit.ServerID = rabbitCfg.ServerID
		cfg.rabbit.ServerKey = rabbitCfg.ServerKey
		rabbitClient, err := rabbit.NewClient(cfg.rabbit, appl, RabbitCertPath)
		if err != nil {
			log.Err("failed to init rabbit client", "error", err)
		} else {
			log.Info("Serve rabbit client")

			appl.InitBonusRabbitWorker(string(vo.WashBonusService), rabbitClient.SendMessage)
			appl.FetchSessions()
		}
	}

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
