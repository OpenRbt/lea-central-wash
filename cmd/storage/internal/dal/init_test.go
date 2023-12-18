package dal

import (
	"strconv"
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	"github.com/powerman/check"
	uuid "github.com/satori/go.uuid"

	"github.com/powerman/gotest/testinit"
	_ "github.com/smartystreets/goconvey/convey"
)

func TestMain(m *testing.M) { testinit.Main(m) }

func init() { testinit.Setup(1, setup) }

func setup() {
	def.Init()
}

func mustParseTime(t string) time.Time {
	date, err := time.Parse(time.RFC3339, t)
	if err != nil {
		panic(err.Error())
	}
	return date
}

var (
	testPayment1 = app.Payment{
		ServerID:         uuid.NewV4(),
		OrderID:          uuid.NewV4(),
		PostID:           3,
		UrlPay:           "test url",
		Amount:           0,
		Canceled:         false,
		Confirmed:        false,
		OpenwashReceived: false,
		CreatedAt:        mustParseTime("2023-11-20T03:01:00Z"),
		UpdatedAt:        mustParseTime("2023-11-20T03:02:00Z"),
	}
)

func addTestData(t *check.C) {
	for i := 1; i < 13; i++ {
		err := testRepo.AddStation("Station" + strconv.Itoa(i))
		t.Nil(err)
	}

}
