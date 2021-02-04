package dal

import (
	"context"
	"database/sql"
	"fmt"
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

func (r *repo) Users() (users []app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resUser
		err := tx.NamedSelectContext(ctx, &res, sqlGetUser, argGetUser{})
		if err != nil {
			return err
		}
		users = appSetUser(res)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) Load(stationID app.StationID, key string) (value string, err error) {
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

func (r *repo) Save(stationID app.StationID, key string, value string) (err error) {
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

func (r *repo) SaveIfNotExists(stationID app.StationID, key string, value string) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetValueIfNotExists, argSetValue{
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
		_, err = tx.NamedExec(sqlUpdStation, argUpdStation{
			ID:   station.ID,
			Name: station.Name,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) AddStation(name string) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err = tx.NamedExec(sqlAddStation, argAddStation{
			Name: name,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) DelStation(id app.StationID) (err error) {
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

func (r *repo) LoadHash() (ids []app.StationID, hash []string, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resLoadHash
		err := tx.NamedSelectContext(ctx, &res, sqlLoadHash, argGetStation{})
		if err != nil {
			return err
		}
		for i := range res {
			ids = append(ids, res[i].StationID)
			hash = append(hash, res[i].Hash)
		}
		return nil
	})
	return //nolint:nakedret
}

// Info returns database information
func (r *repo) Info() string {
	return "postgres"
}

func (r *repo) LastMoneyReport(stationID app.StationID) (report app.MoneyReport, err error) {
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
		_, err := tx.NamedExec(sqlAddMoneyReport, argAddMoneyReport{
			StationID:    report.StationID,
			Banknotes:    report.Banknotes,
			CarsTotal:    report.CarsTotal,
			Coins:        report.Coins,
			Electronical: report.Electronical,
			Service:      report.Service,
			Ctime:        time.Now().UTC(),
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) LastRelayReport(stationID app.StationID) (report app.RelayReport, err error) {
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
		for i := range report.RelayStats {
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

func (r *repo) MoneyReport(stationID app.StationID, startDate, endDate time.Time) (report app.MoneyReport, err error) {
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

func (r *repo) CurrentMoney(stationID app.StationID) (report app.MoneyReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &report, sqlCurrentMoney, argCurrentMoney{
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

func (r *repo) RelayStatReport(stationID app.StationID, startDate, endDate time.Time) (report app.RelayReport, err error) {
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

func (r *repo) LastCollectionReport(stationID app.StationID) (report app.CollectionReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &report, sqlLastCollectionReport, argLastCollectionReport{
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

func (r *repo) SaveCollectionReport(userID int, id app.StationID) (err error) {
	fmt.Println("DAL: SaveCollectionReport")
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlAddCollectionReport, argAddCollectionReport{
			StationID: id,
			UserID:    userID,
			Ctime:     time.Now().UTC(),
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) StationsVariables() (stations []app.StationsVariables, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resStationsVariables
		err := tx.NamedSelectContext(ctx, &res, sqlGetStationsVariables, argGetStation{})
		if err != nil {
			return err
		}
		stations = appStationsVariables(res)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) SetHash(id app.StationID, hash string) error {
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlStationNullHash, argStationHash{
			StationID: id,
			Hash:      hash,
		})
		if err != nil {
			return err
		}
		if hash != "" {
			_, err = tx.NamedExec(sqlAddStationHash, argStationHash{
				StationID: id,
				Hash:      hash,
			})
		}
		return err
	})
	return err
}

func (r *repo) AddOpenStationLog(stationID app.StationID) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlOpenStationLogAdd, argOpenStationLogAdd{
			StationID: stationID,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) CheckDB() (ok bool, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resCheckDB
		err := tx.NamedGetContext(ctx, &res, sqlCheckDB, argCheckDB{})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		log.Info("CheckDB", "count", res.CountColumns)
		ok = res.CountColumns == 3
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) Programs(id app.StationID) (programs []app.Program, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resPrograms
		err = tx.NamedSelectContext(ctx, &res, sqlPrograms, argPrograms{
			StationID: id,
		})

		if err == sql.ErrNoRows {
			return nil
		}

		if err != nil {
			return err
		}

		programs = appPrograms(res)

		return nil
	})
	return
}

func (r *repo) ProgramRelays(id app.StationID, programID int) (relays []app.Relay, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var jsonRelays string
		err = tx.NamedGetContext(ctx, &jsonRelays, sqlProgramRelays, argProgramRelays{
			StationID: id,
			ProgramID: programID,
		})

		if err == sql.ErrNoRows {
			return nil
		}

		if err != nil {
			return err
		}

		relays = appProgramRelays(jsonRelays)

		return nil
	})

	return
}

func (r *repo) SetProgramName(id app.StationID, programID int, name string) (err error) {

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err = tx.NamedExec(sqlSetProgramName, argSetProgramName{
			StationID: id,
			ProgramID: programID,
			Name:      name,
		})

		return err
	})

	return
}

func (r *repo) SetProgramRelays(id app.StationID, programID int, relays []app.Relay) (err error) {

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		relaysJSON := dalProgramRelays(relays)

		_, err = tx.NamedExec(sqlSetProgramRelays, argSetProgramRelays{
			StationID: id,
			ProgramID: programID,
			Relays:    relaysJSON,
		})

		return err
	})

	return
}

func (r *repo) Kasse() (kasse app.Kasse, err error) {

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resKasse
		err = tx.NamedGetContext(ctx, &res, sqlKasse, argKasseGet{})

		if err == sql.ErrNoRows {
			return nil
		}

		kasse = appKasse(res)
		return err
	})

	return
}

func (r *repo) SetKasse(kasse app.Kasse) (err error) {

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {

		_, err = tx.NamedExec(sqlSetKasse, argSetKasse{
			CashierFullName: kasse.CashierFullName,
			CashierINN:      kasse.CashierINN,
			TaxType:         kasse.TaxType,
			ReceiptItem:     kasse.ReceiptItem,
		})

		return err
	})

	return
}
