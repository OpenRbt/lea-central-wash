package dal

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"runtime"
	"strings"
	"sync"
	"time"

	uuid "github.com/satori/go.uuid"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/jmoiron/sqlx"
	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/powerman/pqx"
	"github.com/powerman/sqlxx"
	"github.com/powerman/structlog"
)

// Ctx is a synonym for convenience.
type Ctx = context.Context

var log = structlog.New() //nolint:gochecknoglobals

var ctx = context.Background() //nolint:gochecknoglobals

func pqErrConflictIn(err error, constraint string) bool {
	pqErr, ok := err.(*pq.Error)
	return ok && pqErr.Constraint == constraint
}

func rollback(tx *sqlxx.Tx) {
	if err := tx.Rollback(); err != nil && err != sql.ErrTxDone {
		log.Warn("failed to tx.Rollback", "err", err)
	}
}

type repo struct {
	db            *sqlxx.DB
	maintenanceDB *sqlxx.DB
	PaymentsRep
}

var _ = app.Repo(&repo{})

// New creates and returns new Repo.
func New(db *sqlx.DB, maintenanceDB *sqlx.DB) *repo {
	return &repo{
		db:            sqlxx.NewDB(db),
		maintenanceDB: sqlxx.NewDB(maintenanceDB),
		PaymentsRep: PaymentsRep{
			RWMutex:     &sync.RWMutex{},
			LastPayment: make(map[int]*app.Payment),
		},
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

func (r *repo) txMaintenance(ctx Ctx, opts *sql.TxOptions, f func(*sqlxx.Tx) error) (err error) {
	pc, _, _, _ := runtime.Caller(2)
	names := strings.Split(runtime.FuncForPC(pc).Name(), ".")
	methodName := names[len(names)-1]
	return pqx.Serialize(func() error {
		tx, err := r.maintenanceDB.BeginTxx(ctx, opts)
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

// Close closes connection to DB.
func (r *repo) Close() {
	log.WarnIfFail(r.db.Close)
}

func (r *repo) User(login string) (user app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &user, sqlGetUser, argGetUser{
			Login: login,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) Users() (users []app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resUser
		err := tx.NamedSelectContext(ctx, &res, sqlGetUsers, argGetUsers{})
		if err != nil {
			return err
		}
		users = appSetUsers(res)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) CreateUser(userData app.UserData) (newUser app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &newUser, sqlAddUser, argAddUser{
			Login:      userData.Login,
			Password:   userData.Password,
			FirstName:  *userData.FirstName,
			MiddleName: *userData.MiddleName,
			LastName:   *userData.LastName,
			IsAdmin:    *userData.IsAdmin,
			IsEngineer: *userData.IsEngineer,
			IsOperator: *userData.IsOperator,
		})
		if pqErrConflictIn(err, constraintUserLogin) {
			return app.ErrLoginNotUnique
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) UpdateUser(userData app.UserData) (newUser app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &newUser, sqlUpdateUser, argUpdateUser{
			Login:      userData.Login,
			FirstName:  *userData.FirstName,
			MiddleName: *userData.MiddleName,
			LastName:   *userData.LastName,
			IsAdmin:    *userData.IsAdmin,
			IsEngineer: *userData.IsEngineer,
			IsOperator: *userData.IsOperator,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) UpdateUserPassword(userData app.UpdatePasswordData) (newUser app.UserData, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &newUser, sqlUpdateUserPassword, argUpdateUserPassword{
			Login:       userData.Login,
			NewPassword: userData.NewPassword,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) DeleteUser(login string) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlDelUser, argDelUser{
			Login: login,
		})
		if pqErrConflictIn(err, constraintMoneyCollection) {
			return app.ErrMoneyCollectionFkey
		}
		return err
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
			ID:           station.ID,
			Name:         station.Name,
			PreflightSec: station.PreflightSec,
			RelayBoard:   station.RelayBoard,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) AddStation(name string) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var id app.StationID
		err = tx.NamedGetContext(ctx, &id, sqlAddStation, argAddStation{
			Name: name,
		})
		if err != nil {
			return err
		}
		_, err := tx.NamedExec(sqlAddCollectionReport, argAddCollectionReport{
			StationID: id,
			UserID:    1,
			Ctime:     time.Now().UTC(),
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
		err := tx.NamedSelectContext(ctx, &res, sqlGetStations, argGetStations{})
		if err != nil {
			return err
		}
		stations = appSetStation(res)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) Station(id app.StationID) (station app.SetStation, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resStation
		err := tx.NamedGetContext(ctx, &res, sqlGetStation, argGetStation{
			ID: id,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		station = appStation(res)
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
			Bonuses:      report.Bonuses,
			Ctime:        time.Now().UTC(),
			SessionID:    report.SessionID,
			QrMoney:      report.QrMoney,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) ResetStationStat(stationID app.StationID) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resRelayReport
		err := tx.NamedSelectContext(ctx, &res, sqlCurentStationReport, argStationReport{
			StationID: &stationID,
		})
		if err != nil {
			return err
		}
		var relay []resRelayStats
		err = tx.NamedSelectContext(ctx, &relay, sqlCurentRelayStat, argStationReport{
			StationID: &stationID,
		})
		if err != nil {
			return err
		}
		report := appStationsStat(res, relay)
		stat := report[stationID]
		bytes, err := json.Marshal(stat)
		if err != nil {
			panic(err)
		}
		_, err = tx.NamedExec(sqlResetRelayStat, argResetRelayStat{
			StationID: stationID,
			Ver:       1,
			Data:      string(bytes),
		})

		return err
	})
	return //nolint:nakedret
}

func (r *repo) RelayReportCurrent(stationID *app.StationID) (report app.StationsStat, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resRelayReport
		err := tx.NamedSelectContext(ctx, &res, sqlCurentStationReport, argStationReport{
			StationID: stationID,
		})
		if err != nil {
			return err
		}
		var relay []resRelayStats
		err = tx.NamedSelectContext(ctx, &relay, sqlCurentRelayStat, argStationReport{
			StationID: stationID,
		})
		if err != nil {
			return err
		}
		report = appStationsStat(res, relay)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) RelayReportDates(stationID *app.StationID, startDate, endDate time.Time) (report app.StationsStat, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resRelayReport
		err := tx.NamedSelectContext(ctx, &res, sqlDatesStationReport, argDatesStationStat{
			StationID: stationID,
			StartDate: startDate,
			EndDate:   endDate,
		})
		if err != nil {
			return err
		}
		var relay []resRelayStats
		err = tx.NamedSelectContext(ctx, &relay, sqlDatesRelayStat, argDatesStationStat{
			StationID: stationID,
			StartDate: startDate,
			EndDate:   endDate,
		})
		if err != nil {
			return err
		}
		report = appStationsStat(res, relay)
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
		err = stmt.Get(&id, argAddRelayReport{
			StationID:  report.StationID,
			ProgramID:  report.ProgramID,
			PumpTimeOn: report.PumpTimeOn,
			TimeOn:     report.TimeOn,
			Ctime:      time.Now().UTC(),
		})
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
func (r *repo) CollectionReports(id app.StationID, startDate, endDate *time.Time) (reports []app.CollectionReportWithUser, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resCollectionReportByDate
		err := tx.NamedSelectContext(ctx, &res, sqlCollectionReportsByDate, argCollectionReportsByDate{
			StationID: id,
			StartDate: startDate,
			EndDate:   endDate,
		})
		reports = appCollectionReportsByDate(res)
		if err != nil {
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

func (r *repo) GetPrograms(ctx context.Context, filter app.ProgramFilter) ([]app.Program, int64, error) {
	var resPrograms []resProgram
	var count int64

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &resPrograms, sqlPrograms.
			OrderBy("id").
			WithPagination(filter.Pagination).String(),
			argPrograms{ID: filter.ID})
		if err != nil {
			return err
		}

		if len(resPrograms) > 0 {
			count = resPrograms[0].TotalCount
		} else if filter.Offset() > 0 {
			return tx.NamedGetContext(ctx, &count, sqlPrograms.Count().String(), argPrograms{ID: filter.ID})
		}

		return nil
	})
	if err != nil {
		return nil, 0, err
	}

	return appPrograms(resPrograms), count, nil
}

func (r *repo) SetProgram(ctx context.Context, program app.Program) (app.Program, error) {
	var resProgram resProgram
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.NamedGetContext(ctx, &resProgram, sqlSetProgram, argSetProgram{
			ID:                         program.ID,
			Name:                       program.Name,
			Price:                      program.Price,
			PreflightEnabled:           program.PreflightEnabled,
			MotorSpeedPercent:          program.MotorSpeedPercent,
			PreflightMotorSpeedPercent: program.PreflightMotorSpeedPercent,
			IsFinishingProgram:         program.IsFinishingProgram,
			Relays:                     dalProgramRelays(program.Relays),
			PreflightRelays:            dalProgramRelays(program.PreflightRelays),
		})
	})
	if err != nil {
		return app.Program{}, err
	}

	return appProgram(resProgram), nil
}

func (r *repo) StationProgram(id app.StationID) (button []app.StationProgram, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resStationProgram
		err = tx.NamedSelectContext(ctx, &res, sqlStationProgram, argStationProgram{
			StationID: id,
		})

		if err != nil {
			return err
		}

		button = appStationProgram(res)

		return nil
	})

	return
}

func (r *repo) SetStationProgram(id app.StationID, button []app.StationProgram) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err = tx.NamedExec(sqlStationProgramDel, argStationProgramDel{
			StationID: id,
		})
		if err != nil {
			return err
		}
		for i := range button {
			_, err = tx.NamedExec(sqlStationProgramAdd, argStationProgramAdd{
				StationID: id,
				ButtonID:  button[i].ButtonID,
				ProgramID: button[i].ProgramID,
			})
			if pqErrConflictIn(err, constraintStationProgramID) {
				return app.ErrUnknownProgram
			}
			if pqErrConflictIn(err, constraintStationStationID) {
				return app.ErrUnknownStation
			}
			if pqErrConflictIn(err, constraintStationProgramUnique) {
				return app.ErrStationProgramMustUnique
			}
			if err != nil {
				return err
			}
		}
		return nil
	})

	return
}

func (r *repo) StationConfig(id app.StationID) (cfg app.StationConfig, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resStationConfig
		err = tx.NamedSelectContext(ctx, &res, sqlStationConfig, argStationConfig{
			ID: id,
		})
		if err != nil {
			return err
		}
		cfg = appStationConfig(res)
		return nil
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

func (r *repo) CardReaderConfig(stationID app.StationID) (cfg *app.CardReaderConfig, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resGetCardReaderConfig
		err := tx.NamedGetContext(ctx, &res, sqlGetCardReaderConfig, argGetCardReaderConfig{
			StationID: stationID,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		cfg = &app.CardReaderConfig{
			StationID:      res.StationID,
			CardReaderType: res.CardReaderType,
			Host:           res.Host,
			Port:           res.Port,
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) SetCardReaderConfig(cfg app.CardReaderConfig) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetCardReaderConfig, argSetCardReaderConfig{
			StationID:      cfg.StationID,
			CardReaderType: cfg.CardReaderType,
			Host:           cfg.Host,
			Port:           cfg.Port,
		})
		if pqErrConflictIn(err, constraintCardReaderStationID) {
			return app.ErrNotFound
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) AddUpdateConfig(note string) (id int, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &id, sqlUpdateConfigAdd, argUpdateConfigAdd{
			Note:  note,
			Ctime: time.Now().UTC(),
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) LastUpdateConfig() (id int, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &id, sqlLastUpdateConfigGet, argLastUpdateConfigGet{})
		if err == sql.ErrNoRows {
			id = 0
			return nil
		}
		return err
	})
	return //nolint:nakedret
}

func (r *repo) AddAdvertisingCampaign(ctx context.Context, a app.AdvertisingCampaign) (app.AdvertisingCampaign, error) {
	var resAdvert resAdvertisingCampaign

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.NamedGetContext(ctx, &resAdvert, sqlAddAdvertisingCampaign, dalAdvertisingCampaign(a))
	})
	if err != nil {
		return app.AdvertisingCampaign{}, err
	}

	return appAdvertisingCampaign(resAdvert), nil
}

func (r *repo) EditAdvertisingCampaign(ctx context.Context, a app.AdvertisingCampaign) (app.AdvertisingCampaign, error) {
	var resAdvert resAdvertisingCampaign

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &resAdvert, sqlEditAdvertisingCampaign, dalAdvertisingCampaign(a))
		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
	if err != nil {
		return app.AdvertisingCampaign{}, err
	}

	return appAdvertisingCampaign(resAdvert), nil
}

func (r *repo) DeleteAdvertisingCampaign(ctx context.Context, id int64) (app.AdvertisingCampaign, error) {
	var resAdvert resAdvertisingCampaign
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &resAdvert, sqlDelAdvertisingCampaign, argID[int64]{ID: id})
		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
	if err != nil {
		return app.AdvertisingCampaign{}, err
	}

	return appAdvertisingCampaign(resAdvert), nil
}

func (r *repo) GetAdvertisingCampaignByID(ctx context.Context, id int64) (app.AdvertisingCampaign, error) {
	var resAdvert resAdvertisingCampaign
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &resAdvert, sqlAdvertisingCampaignByID, argID[int64]{ID: id})
		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
	if err != nil {
		return app.AdvertisingCampaign{}, err
	}

	return appAdvertisingCampaign(resAdvert), nil
}

func (r *repo) GetAdvertisingCampaigns(ctx context.Context, filter app.AdvertisingCampaignFilter) ([]app.AdvertisingCampaign, int64, error) {
	var resAdverts []resAdvertisingCampaign
	var count int64

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &resAdverts, sqlAdvertisingCampaign.
			OrderBy("id").
			WithPagination(filter.Pagination).String(),
			dalAdvertisingCampaignFilter(filter))
		if err != nil {
			return err
		}

		if len(resAdverts) > 0 {
			count = resAdverts[0].TotalCount
		} else if filter.Offset() > 0 {
			return tx.NamedGetContext(ctx, &count, sqlAdvertisingCampaign.Count().String(), dalAdvertisingCampaignFilter(filter))
		}

		return nil
	})
	if err != nil {
		return nil, 0, err
	}

	return appAdvertisingCampaigns(resAdverts), count, nil
}

func (r *repo) GetCurrentAdvertisingCampaigns(curTime time.Time) (a []app.AdvertisingCampaign, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := []resAdvertisingCampaign{}
		err := tx.NamedSelectContext(ctx, &res, sqlCurrentAdvertisingCampaign, argCurrentAdvertisingCampagins{
			CurrentDate: curTime,
		})
		if err != nil {
			return err
		}
		a = appAdvertisingCampaigns(res)
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) GetConfigInt(name string) (cfg app.ConfigInt, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetConfigInt{}
		err := tx.NamedGetContext(ctx, &res, sqlGetConfigInt, argGetConfig{
			Name: name,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		if err != nil {
			return err
		}
		cfg = appConfigInt(res)
		return nil
	})
	return
}
func (r *repo) GetConfigBool(name string) (cfg app.ConfigBool, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetConfigBool{}
		err := tx.NamedGetContext(ctx, &res, sqlGetConfigBool, argGetConfig{
			Name: name,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		if err != nil {
			return err
		}
		cfg = appConfigBool(res)
		return nil
	})
	return
}
func (r *repo) GetConfigString(name string) (cfg app.ConfigString, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetConfigString{}
		err := tx.NamedGetContext(ctx, &res, sqlGetConfigString, argGetConfig{
			Name: name,
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		if err != nil {
			return err
		}
		cfg = appConfigString(res)
		return nil
	})
	return
}

func (r *repo) SetConfigInt(config app.ConfigInt) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetConfigInt, argSetConfigInt{
			Name:        config.Name,
			Value:       config.Value,
			Description: config.Description,
			Note:        config.Note,
		})
		return err
	})
	return
}

func (r *repo) SetConfigIntIfNotExists(config app.ConfigInt) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetConfigIntIfNotExists, argSetConfigInt{
			Name:        config.Name,
			Value:       config.Value,
			Description: config.Description,
			Note:        config.Note,
		})
		return err
	})
	return
}

func (r *repo) SetConfigBool(config app.ConfigBool) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetConfigBool, argSetConfigBool{
			Name:        config.Name,
			Value:       config.Value,
			Description: config.Description,
			Note:        config.Note,
		})
		return err
	})
	return
}
func (r *repo) SetConfigString(config app.ConfigString) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetConfigString, argSetConfigString{
			Name:        config.Name,
			Value:       config.Value,
			Description: config.Description,
			Note:        config.Note,
		})
		return err
	})
	return
}

func (r *repo) DeleteConfigString(name string) error {
	_, err := r.db.NamedExec(sqlDeleteConfigString, argGetConfig{
		Name: name,
	})

	return err
}

func (r *repo) GetStationConfigInt(name string, stationID app.StationID) (cfg app.StationConfigVar[int64], err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetStationConfigVar[int64]{}
		err := tx.NamedGetContext(ctx, &res, sqlGetStationConfigInt, argGetStationConfig{
			Name:      name,
			StationID: stationID,
		})
		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}
		cfg = appStationConfigVar(res)
		return nil
	})
	return
}
func (r *repo) GetStationConfigBool(name string, stationID app.StationID) (cfg app.StationConfigVar[bool], err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetStationConfigVar[bool]{}
		err := tx.NamedGetContext(ctx, &res, sqlGetStationConfigBool, argGetStationConfig{
			Name:      name,
			StationID: stationID,
		})
		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}
		cfg = appStationConfigVar(res)
		return nil
	})
	return
}
func (r *repo) GetStationConfigString(name string, stationID app.StationID) (cfg app.StationConfigVar[string], err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := resGetStationConfigVar[string]{}
		err := tx.NamedGetContext(ctx, &res, sqlGetStationConfigString, argGetStationConfig{
			Name:      name,
			StationID: stationID,
		})
		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}
		cfg = appStationConfigVar(res)
		return nil
	})
	return
}

func (r *repo) SetStationConfigInt(config app.StationConfigVar[int64]) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetStationConfigInt, dalStationConfigVar(config))
		if pqErrConflictIn(err, constraintStationIntStationID) {
			return app.ErrNotFound
		}
		return err
	})
	return
}

func (r *repo) SetStationConfigBool(config app.StationConfigVar[bool]) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetStationConfigBool, dalStationConfigVar(config))
		if pqErrConflictIn(err, constraintStationBoolStationID) {
			return app.ErrNotFound
		}
		return err
	})
	return
}

func (r *repo) SetStationConfigString(config app.StationConfigVar[string]) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlSetStationConfigString, dalStationConfigVar(config))
		if pqErrConflictIn(err, constraintStationStringStationID) {
			return app.ErrNotFound
		}
		return err
	})
	return
}

func (r *repo) AddRabbitMessage(message app.RabbitMessage) (err error) {
	bytes, ok := message.Payload.([]byte)
	if !ok {
		return app.ErrRabbitMessageBadPayload
	}
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlAddRabbitMessage, argAddRabbitMessage{
			MessageType: message.MessageType,
			Payload:     bytes,
			CreatedAt:   time.Now().UTC(),
		})

		return err
	})
	return
}

func (r *repo) GetUnsendedRabbitMessages(lastMessageID int64) (messages []app.RabbitMessage, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resRabbitMessage
		err := tx.NamedSelectContext(ctx, &res, sqlGetUnsendedRabbitMessages, argGetUnsendedRabbitMessages{
			LastID: lastMessageID,
		})

		if err != nil {
			return err
		}

		messages = appRabbitMessages(res)

		return nil
	})
	return
}

func (r *repo) MarkRabbitMessageAsSent(id int64) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlMarkRabbitMessageAsSent, argMarkRabbitMessageAsSent{
			ID:    id,
			Ctime: time.Now().UTC(),
		})

		return err
	})

	return
}

func (r *repo) GetUnsendedMoneyReports(lastMessageID int64) (rabbitMoneyReports []app.RabbitMoneyReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res := []resRabbitMoneyReport{}
		err := tx.NamedSelectContext(ctx, &res, sqlGetUnsendedRabbitMoneyReports, argGetUnsendedRabbitMessages{
			LastID: lastMessageID,
		})

		if err != nil {
			return err
		}

		rabbitMoneyReports = appRabbitMoneyReports(res)

		return nil
	})
	return
}

func (r *repo) SaveMoneyReportAndMessage(report app.RabbitMoneyReport) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		now := time.Now().UTC()
		id := 0
		stmt, err := tx.PrepareNamed(sqlAddMoneyReportForRabbitMessage)
		if err != nil {
			return err
		}

		err = stmt.Get(&id, argAddMoneyReport{
			StationID:    report.MoneyReport.StationID,
			Banknotes:    report.MoneyReport.Banknotes,
			CarsTotal:    report.MoneyReport.CarsTotal,
			Coins:        report.MoneyReport.Coins,
			Electronical: report.MoneyReport.Electronical,
			Service:      report.MoneyReport.Service,
			Bonuses:      report.MoneyReport.Bonuses,
			Ctime:        now,
			SessionID:    report.MoneyReport.SessionID,
			QrMoney:      report.MoneyReport.QrMoney,
		})

		if err != nil {
			return err
		}

		_, err = tx.NamedExec(sqlAddRabbitMoneyReport, argAddRabbitMoneyReport{
			MessageType:   report.MessageType,
			MoneyReportID: id,
			CreatedAt:     now,
			MessageUUID:   uuid.NullUUID{UUID: uuid.NewV4(), Valid: true},
		})

		return err
	})
	return
}

func (r *repo) MarkRabbitMoneyReportAsSent(id int64) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlMarkRabbitMoneyReportAsSent, argMarkRabbitMessageAsSent{
			ID:    id,
			Ctime: time.Now().UTC(),
		})

		return err
	})

	return
}

func (r *repo) RefreshMotorStatsCurrent() (err error) {
	err = r.txMaintenance(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(sqlRefreshMotorStatsCurrent)
		if err != nil {
			return err
		}

		_, err = tx.Exec(sqlRefreshProgramStatsCurrent)

		return err
	})

	return
}

func (r *repo) RefreshMotorStatsDates() (err error) {
	err = r.txMaintenance(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(sqlRefreshMotorStatsDates)
		if err != nil {
			return err
		}

		_, err = tx.Exec(sqlRefreshProgramStatsDates)

		return err
	})

	return
}

func (r *repo) CreateOpenwashingLog(model app.OpenwashingLogCreate) (app.OpenwashingLog, error) {
	var log app.OpenwashingLog

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res respOpenwashingLog
		err := tx.NamedGetContext(ctx, &res, sqlInsertOpenwashingLog, argInsertOpenwashingLog{
			StationID: int(model.StationID),
			Text:      model.Text,
			Type:      model.Type,
			Level:     dalLogLevel(model.Level),
			CreatedAt: time.Now().UTC(),
		})
		if err != nil {
			return err
		}

		log = appOpenwashingLog(res)

		return nil
	})

	if err != nil {
		return app.OpenwashingLog{}, err
	}

	return log, err
}
