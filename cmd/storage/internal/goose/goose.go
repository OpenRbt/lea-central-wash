package goose

import (
	"database/sql"
	"regexp"
	"strings"

	"github.com/pkg/errors"
	"github.com/powerman/structlog"
	"github.com/pressly/goose"
)

// Usage list goose commands.
const Usage = `
COMMAND for -goose should be a single string with one or more words:
    up                   Migrate the DB to the most recent version available
    up-by-one            Migrate the DB up by 1
    up-to VERSION        Migrate the DB to a specific VERSION
    down                 Roll back the version by 1
    down-to VERSION      Roll back to a specific VERSION
    redo                 Re-run the latest migration
    reset                Roll back all migrations
    status               Dump the migration status for the current DB
    version              Print the current version of the database
    create NAME [sql|go] Creates new migration file with the current timestamp
    fix                  Apply sequential ordering to migrations
`

var reCommand = regexp.MustCompile(`^(up|up-by-one|up-to\s+\d+|down|down-to\s+\d+|redo|reset|status|version|create\s+\S+\s+(go|sql)|fix)$`) //nolint:gochecknoglobals

// Init must be called once before using this package.
func Init(dialect string) {
	log := structlog.New().
		SetDefaultKeyvals(structlog.KeyUnit, "goose").
		SetKeysFormat(map[string]string{structlog.KeyMessage: " %[2]s"})
	err := goose.SetDialect(dialect)
	if err != nil {
		panic(err)
	}
	goose.SetLogger(log)
}

// ValidCommand returns true if command and is valid goose command.
func ValidCommand(command string) bool {
	return reCommand.MatchString(command)
}

// Run executes goose command. It also enforce "fix" after "create".
func Run(db *sql.DB, dir string, command string) error {
	cmdArgs := strings.Fields(command)
	cmd, args := cmdArgs[0], cmdArgs[1:]
	err := goose.Run(cmd, db, dir, args...)
	if err == nil && cmd == "create" {
		return Run(db, dir, "fix")
	}
	return errors.Wrapf(err, "failed to run goose %q", command)
}

// UpTo migrates up to a specific version.
//
// Unlike goose.UpTo it will return error is current version doesn't match
// requested one after migration.
func UpTo(db *sql.DB, dir string, version int64) error {
	err := goose.UpTo(db, dir, version)
	if err != nil {
		return errors.Wrapf(err, "failed to migrate up to %d", version)
	}
	if v, _ := goose.GetDBVersion(db); v != version { // TODO replace with narada4d
		return errors.Errorf("require db schema version %d, current is %d", version, v)
	}
	return nil
}
