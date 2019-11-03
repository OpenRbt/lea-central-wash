package dal

import (
	"context"
	"database/sql"
	"runtime"
	"strconv"
	"strings"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/migration"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/powerman/narada4d/schemaver"
	"github.com/powerman/pqx"
	"github.com/powerman/sqlxx"
	"github.com/powerman/structlog"
)

// Ctx is a synonym for convenience.
type Ctx = context.Context

var log = structlog.New() //nolint:gochecknoglobals

var ctx = context.Background() //nolint:gochecknoglobals

var errSchemaVer = errors.New("unsupported DB schema version")

func rollback(tx *sqlxx.Tx) {
	if err := tx.Rollback(); err != nil && err != sql.ErrTxDone {
		log.Warn("failed to tx.Rollback", "err", err)
	}
}

type repo struct {
	db        *sqlxx.DB
	schemaVer *schemaver.SchemaVer
}

// New creates and returns new Repo.
func New(db *sqlx.DB, schemaVer *schemaver.SchemaVer) app.Repo {
	return &repo{
		db:        sqlxx.NewDB(db),
		schemaVer: schemaVer,
	}
}

func (r *repo) tx(ctx Ctx, opts *sql.TxOptions, f func(*sqlxx.Tx) error) (err error) {
	pc, _, _, _ := runtime.Caller(2)
	names := strings.Split(runtime.FuncForPC(pc).Name(), ".")
	methodName := names[len(names)-1]
	return pqx.Serialize(r.schemaLock(func() error {
		tx, err := r.db.BeginTxx(ctx, opts)
		if err == nil {
			defer rollback(tx)
			err = errors.Wrap(f(tx), methodName)
		}
		if err == nil {
			return tx.Commit()
		}
		return err
	}))
}

func (r *repo) schemaLock(f func() error) func() error {
	target := strconv.Itoa(migration.CurrentVersion)
	return func() error {
		if ver := r.schemaVer.SharedLock(); ver != target {
			return errors.Wrapf(errSchemaVer, "schema version %s, need %s", ver, target)
		}
		defer r.schemaVer.Unlock()
		return f()
	}
}

func (r *repo) Load(stationID string, key string) (value []byte, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resGetValue
		err := tx.NamedGetContext(ctx, &res, sqlGetValue, argGetValue{
			StationID: stationID,
			Key:       key,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		value = res.Value
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) Save(stationID string, key string, value []byte) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetValue, argSetValue{
			StationID: stationID,
			Key:       key,
			Value:     value,
		})
		return err
	})
	return //nolint:nakedret
}
