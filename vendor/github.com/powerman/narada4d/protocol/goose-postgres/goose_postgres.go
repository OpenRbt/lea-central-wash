package goosepostgres

import (
	"database/sql"
	"errors"
	"net/url"
	"strconv"

	"github.com/powerman/must"
	"github.com/powerman/narada4d/schemaver"
	_ "github.com/powerman/pqx" //nolint:golint
	"github.com/pressly/goose"
)

const (
	sqlInitialized   = `SELECT COUNT(*) FROM goose_db_version`
	sqlSharedLock    = `LOCK TABLE goose_db_version IN SHARE MODE`
	sqlExclusiveLock = `LOCK TABLE goose_db_version IN SHARE UPDATE EXCLUSIVE MODE`
)

type storage struct {
	db *sql.DB
	tx *sql.Tx
}

func init() {
	schemaver.RegisterProtocol("goose-postgres", schemaver.Backend{
		Initialize: initialize,
		New:        newStorage,
	})
}

func initialized(db *sql.DB) bool {
	var count int
	_ = db.QueryRow(sqlInitialized).Scan(&count)
	return count > 0
}

func initialize(loc *url.URL) error {
	manage, err := newStorage(loc)
	if err != nil {
		return err
	}
	s := manage.(*storage)
	defer s.db.Close() //nolint:errcheck

	if initialized(s.db) {
		return errors.New("already initialized")
	}

	_, err = goose.EnsureDBVersion(s.db)
	return err
}

func newStorage(loc *url.URL) (schemaver.Manage, error) {
	loc.Scheme = "postgres"
	db, err := sql.Open("pqx", loc.String())
	if err != nil {
		return nil, err
	}
	err = db.Ping()
	if err != nil {
		return nil, err
	}

	err = goose.SetDialect("postgres")
	if err != nil {
		return nil, err
	}

	return &storage{db: db}, nil
}

func (s *storage) SharedLock() {
	if s.tx != nil {
		panic("already locked")
	}
	var err error
	s.tx, err = s.db.Begin()
	must.PanicIf(err)
	_, err = s.tx.Exec(sqlSharedLock)
	must.PanicIf(err)
}

func (s *storage) ExclusiveLock() {
	if s.tx != nil {
		panic("already locked")
	}
	var err error
	s.tx, err = s.db.Begin()
	must.PanicIf(err)
	_, err = s.tx.Exec(sqlExclusiveLock)
	must.PanicIf(err)
}

func (s *storage) Unlock() {
	if s.tx == nil {
		panic("not locked")
	}
	must.PanicIf(s.tx.Commit())
	s.tx = nil
}

func (s *storage) Get() string {
	if s.tx == nil {
		panic("not locked")
	}
	v, err := goose.EnsureDBVersion(s.db)
	must.PanicIf(err)
	if v == 0 {
		return "none"
	}
	return strconv.Itoa(int(v))
}

func (s *storage) Set(string) {
	panic("not supported")
}

func (s *storage) Close() error {
	if s.tx != nil {
		return errors.New("locked")
	}
	return s.db.Close()
}
