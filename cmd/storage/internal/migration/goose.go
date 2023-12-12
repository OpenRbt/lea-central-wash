// Package migration defines DB migrations.
package migration

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

	_ "github.com/lib/pq" // Driver.
	"github.com/powerman/must"
	_ "github.com/powerman/narada4d/protocol/goose-postgres" // Driver.
	"github.com/powerman/narada4d/schemaver"
	"github.com/powerman/pqx"
	"github.com/powerman/structlog"
	"github.com/pressly/goose"
)

const (
	dbTimeout     = 1 * time.Second
	dbIdleTimeout = 1 * time.Second
)

//nolint:gochecknoglobals
var (
	log     = structlog.New().SetDefaultKeyvals(structlog.KeyUnit, "migrate")
	gooseMu sync.Mutex
)

func Connect(ctx context.Context, cfg pqx.Config) (*sql.DB, error) {
	goose.SetLogger(structlog.New().
		SetDefaultKeyvals(structlog.KeyUnit, "goose").
		SetKeysFormat(map[string]string{structlog.KeyMessage: " %[2]s"}))
	must.NoErr(goose.SetDialect("postgres"))

	if deadline, ok := ctx.Deadline(); ok && cfg.ConnectTimeout == 0 {
		cfg.ConnectTimeout = time.Until(deadline)
		if cfg.ConnectTimeout <= 0 {
			return nil, context.DeadlineExceeded
		}
	}

	cfg.DefaultTransactionIsolation = sql.LevelDefault
	cfg.StatementTimeout = dbTimeout
	cfg.LockTimeout = dbTimeout
	cfg.IdleInTransactionSessionTimeout = dbIdleTimeout

	db, err := sql.Open("postgres", cfg.FormatDSN())
	if err != nil {
		return nil, fmt.Errorf("sql.Open: %w", err)
	}

	err = db.PingContext(ctx)
	for err != nil {
		nextErr := db.PingContext(ctx)
		if errors.Is(nextErr, context.DeadlineExceeded) || errors.Is(nextErr, context.Canceled) {
			log.WarnIfFail(db.Close)
			return nil, fmt.Errorf("db.Ping: %w", err)
		}
		err = nextErr
	}

	return db, nil
}

// UpTo migrates up to a specific version.
//
// Unlike goose.UpTo it will return error is current version doesn't match
// requested one after migration.
func UpTo(ctx context.Context, dir string, version int64, cfg pqx.Config) (*schemaver.SchemaVer, error) {
	gooseMu.Lock()
	defer gooseMu.Unlock()

	db, err := Connect(ctx, cfg)
	if err != nil {
		return nil, err
	}
	defer log.WarnIfFail(db.Close)

	err = goose.UpTo(db, dir, version)
	if err != nil {
		return nil, fmt.Errorf("goose.UpTo %d: %w", version, err)
	}
	if v, _ := goose.GetDBVersion(db); v != version {
		return nil, fmt.Errorf("require db schema version %d, current is %d", version, v)
	}

	must.NoErr(os.Setenv(schemaver.EnvLocation, "goose-"+cfg.FormatURL()))
	return schemaver.New()
}

// Run executes goose command. It also enforce "fix" after "create".
func Run(ctx context.Context, dir string, command string, cfg pqx.Config) error {
	gooseMu.Lock()
	defer gooseMu.Unlock()

	db, err := Connect(ctx, cfg)
	if err != nil {
		return err
	}
	defer log.WarnIfFail(db.Close)

	cmdArgs := strings.Fields(command)
	cmd, args := cmdArgs[0], cmdArgs[1:]
	err = goose.Run(cmd, db, dir, args...)
	if err == nil && cmd == "create" {
		err = goose.Run("fix", db, dir)
	}
	if err != nil {
		return fmt.Errorf("goose.Run %q: %w", command, err)
	}
	return nil
}
