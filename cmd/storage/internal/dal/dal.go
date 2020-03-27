package dal

import (
	"context"
	"database/sql"
	"runtime"
	"strconv"
	"strings"
	"time"

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
	return pqx.Serialize(func() error {
		tx, err := r.db.BeginTxx(ctx, opts)
		if err == nil {
			defer rollback(tx)
			err = errors.Wrap(f(tx), methodName)
		}
		if err == nil {
			return tx.Commit()
		}
		return err
	})
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

func (r *repo) Load(stationID int, key string) (value string, err error) {
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

func (r *repo) Save(stationID int, key string, value string) (err error) {
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

func (r *repo) SetStation(station app.SetStation) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlStationNullHash, argStationNullHash{
			Hash: station.Hash,
		})
		if err != nil {
			return err
		}
		if station.ID == 0 {
			_, err := tx.NamedExec(sqlAddStation, argAddStation{
				Hash: station.Hash,
				Name: station.Name,
			})
			return err
		}
		_, err = tx.NamedExec(sqlUpdStation, argUpdStation{
			ID:   station.ID,
			Hash: station.Hash,
			Name: station.Name,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) DelStation(id int) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlDelStation, argDelStation{
			ID: id,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) Stations() (stations []app.SetStation, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resStation
		err := tx.NamedSelectContext(ctx, &res, sqlGetStation, argGetStation{})
		if err != nil {
			return err
		}
		stations = appSetStation(res)
		return nil
	})
	return //nolint:nakedret
}

// Info returns database information
func (r *repo) Info() string {
	return "postgres"
}

func (r *repo) LastMoneyReport(stationID int) (report app.MoneyReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &report, sqlLastMoneyReport, argLastMoneyReport{
			StationID: stationID,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) SaveMoneyReport(report app.MoneyReport) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlAddMoneyReport, report)
		return err
	})
	return //nolint:nakedret
}

func (r *repo) LastRelayReport(stationID int) (report app.RelayReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resRelayReport
		err := tx.NamedGetContext(ctx, &res, sqlLastRelayReport, argLastRelayReport{
			StationID: stationID,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		report.StationID = res.ID
		err = tx.NamedSelectContext(ctx, &report.RelayStats, sqlRelayStat, argRelayStat{
			RelayReportID: res.ID,
		})
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) SaveRelayReport(report app.RelayReport) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		id := 0
		stmt, err := tx.PrepareNamed(sqlAddRelayReport)
		if err != nil {
			return err
		}
		err = stmt.Get(&id, argAddRelayReport{StationID: report.StationID})
		if err != nil {
			return err
		}
		for i, _ := range report.RelayStats {
			r := argAddRelayStat{
				RelayReportID: id,
				RelayID:       report.RelayStats[i].RelayID,
				SwitchedCount: report.RelayStats[i].SwitchedCount,
				TotalTimeOn:   report.RelayStats[i].TotalTimeOn,
			}
			_, err = tx.NamedExec(sqlAddRelayStat, r)
			if err != nil {
				return err
			}
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) MoneyReport(stationID int, startDate, endDate time.Time) (report app.MoneyReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &report, sqlMoneyReport, argMoneyReport{
			StationID: stationID,
			StartDate: startDate,
			EndDate:   endDate,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) RelayStatReport(stationID int, startDate, endDate time.Time) (report app.RelayReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &report.RelayStats, sqlRelayStatReport, argRelayStatReport{
			StationID: stationID,
			StartDate: startDate,
			EndDate:   endDate,
		})
		return err
	})
	return //nolint:nakedret
}
