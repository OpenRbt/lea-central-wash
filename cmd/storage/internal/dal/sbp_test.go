//go:build integration

package dal

import (
	"database/sql"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/gofrs/uuid"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/powerman/narada4d/schemaver"
	"github.com/powerman/pqx"
)

type testSbp struct {
	db *repo
}

// getDalConfigForTests ...
func (t *testSbp) getDalConfigForTests() (err error) {

	getIntEnv := func(env string) int {
		resp, err := strconv.ParseInt(os.Getenv(env), 10, 64)
		if err != nil {
			return 0
		}
		return int(resp)
	}

	dbConf := pqx.Config{
		DBName:                      os.Getenv("TEST_DB_NAME"),
		User:                        os.Getenv("TEST_DB_USER"),
		Pass:                        os.Getenv("TEST_DB_PASS"),
		Host:                        os.Getenv("TEST_DB_HOST"),
		Port:                        getIntEnv("TEST_DB_PORT"),
		SSLMode:                     pqx.SSLDisable,
		DefaultTransactionIsolation: sql.LevelSerializable,
	}

	// open
	dbConn, err := sql.Open("pqx", dbConf.FormatDSN())
	if err != nil {
		return err
	}

	// ping
	err = dbConn.PingContext(ctx)
	if err != nil {
		return errors.Wrap(err, "test connect to postgres")
	}

	schemaVer, _ := schemaver.New()

	// transform to sqlx db
	sqlxDb := sqlx.NewDb(dbConn, "postgres")

	t.db = New(sqlxDb, sqlxDb, schemaVer)

	return nil
}

func TestPaymentCase(t *testing.T) {
	testSbp := &testSbp{}
	err := testSbp.getDalConfigForTests()
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	uuidNew, _ := uuid.NewV4()
	req := app.Payment{
		ServerID:         uuidNew,
		OrderID:          uuidNew,
		PostID:           "3",
		UrlPay:           "test url",
		Amount:           0,
		Canceled:         false,
		Confirmed:        false,
		OpenwashReceived: false,
		CreatedAt:        time.Now().UTC(),
		UpdatedAt:        time.Now().UTC(),
	}

	// save
	err = testSbp.db.SavePayment(req)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}

	// get
	p, err := testSbp.db.GetPaymentByOrderID(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	if p != req {
		t.Errorf("the payment in the database does not match the one sent for saving, \n\t1:%+v, \n\t2:%+v", req, p)
		t.FailNow()
	}

	// cancel case
	err = testSbp.db.SetPaymentCanceled(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	pCancel, err := testSbp.db.GetPaymentByOrderID(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	if pCancel.Canceled != true {
		t.Error("the payment has not been canceled")
		t.FailNow()
	}

	// confirmed case
	err = testSbp.db.SetPaymentConfirmed(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	pConfirmed, err := testSbp.db.GetPaymentByOrderID(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	if pConfirmed.Confirmed != true {
		t.Error("the payment has not been confirmed")
		t.FailNow()
	}

	// received case
	err = testSbp.db.SetPaymentReceived(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	pReceived, err := testSbp.db.GetPaymentByOrderID(uuidNew)
	if err != nil {
		t.Error(err)
		t.FailNow()
	}
	if pReceived.OpenwashReceived != true {
		t.Error("the payment has not been received")
		t.FailNow()
	}

}
