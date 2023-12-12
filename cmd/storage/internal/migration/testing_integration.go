package migration

import (
	"context"
	"database/sql"
	"net"
	"runtime"
	"strconv"
	"strings"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"

	"github.com/ory/dockertest"
	"github.com/ory/dockertest/docker"
	"github.com/powerman/gotest/testinit"
	"github.com/powerman/pqx"
)

var (
	user   = "postgres"
	pass   = "postgres"
	dbName = "test"
)
var ctx = context.Background()

// TestDBConfig returns config for created temporary database which will
// be automatically removed after all tests. Database name will contains
// caller's package name.
func TestDBConfig() pqx.Config {
	pc, _, _, _ := runtime.Caller(1)
	sanitize := func(r rune) rune {
		if 'a' <= r && r <= 'z' || 'A' <= r && r <= 'Z' {
			return r
		}
		return '_'
	}
	dbSuffix := runtime.FuncForPC(pc).Name()
	dbSuffix = strings.Map(sanitize, dbSuffix[:strings.LastIndex(dbSuffix, ".")])

	port, err := getFreePort()
	if err != nil {
		testinit.Fatal(err)
	}
	dbCfg := pqx.Config{
		DBName:                          dbName + "_" + dbSuffix,
		User:                            user,
		Pass:                            pass,
		Host:                            "localhost",
		Port:                            port,
		ConnectTimeout:                  15 * def.TestSecond,
		SSLMode:                         pqx.SSLDisable,
		SearchPath:                      def.DBSchema,
		StatementTimeout:                15 * def.TestSecond,
		LockTimeout:                     3 * def.TestSecond,
		IdleInTransactionSessionTimeout: 3 * def.TestSecond,
	}
	pool, err := dockertest.NewPool("")
	if err != nil {
		testinit.Fatal(err)
	}

	opts := dockertest.RunOptions{
		Repository: "postgres",
		Tag:        "14.5",
		Env: []string{
			"POSTGRES_USER=" + user,
			"POSTGRES_PASSWORD=" + pass,
			"POSTGRES_DB=" + dbName + "_" + dbSuffix,
		},
		ExposedPorts: []string{"5432"},
		PortBindings: map[docker.Port][]docker.PortBinding{
			"5432": {
				{HostIP: "0.0.0.0", HostPort: strconv.Itoa(port)},
			},
		},
	}

	resource, err := pool.RunWithOptions(&opts)
	if err != nil {
		testinit.Fatal(err)
	}
	cleanup := func() {
		if err := pool.Purge(resource); err != nil {
			testinit.Fatal(err)
		}
	}
	testinit.Teardown(cleanup)

	db, err := sql.Open("postgres", dbCfg.FormatDSN())
	if err != nil {
		testinit.Fatal(err)
	}

	err = db.PingContext(ctx)
	for err != nil {
		nextErr := db.PingContext(ctx)
		if nextErr == context.DeadlineExceeded {
			testinit.Fatal(err)
		}
		err = nextErr
	}
	return dbCfg
}

func getFreePort() (port int, err error) {
	var a *net.TCPAddr
	if a, err = net.ResolveTCPAddr("tcp", "localhost:0"); err == nil {
		var l *net.TCPListener
		if l, err = net.ListenTCP("tcp", a); err == nil {
			defer l.Close()
			return l.Addr().(*net.TCPAddr).Port, nil
		}
	}
	return
}
