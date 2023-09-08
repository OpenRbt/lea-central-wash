package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/user"
	"runtime"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/flags"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/goose"
	"github.com/jmoiron/sqlx"
	_ "github.com/powerman/narada4d/protocol/goose-postgres"
	"github.com/powerman/structlog"
)

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
