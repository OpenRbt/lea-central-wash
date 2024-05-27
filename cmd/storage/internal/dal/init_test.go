package dal

import (
	"strconv"
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	"github.com/powerman/check"
	uuid "github.com/satori/go.uuid"
	"gotest.tools/v3/assert"

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
	testCampaign1 = app.AdvertisingCampaign{
		Name:             "test",
		DefaultDiscount:  10,
		DiscountPrograms: []app.DiscountProgram{{Discount: 1, ProgramID: 1}, {Discount: 2, ProgramID: 2}},
		StartDate:        time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC),
		EndDate:          time.Date(2024, 5, 25, 0, 0, 0, 0, time.UTC),
		StartMinute:      0,
		EndMinute:        60,
		Weekday:          []string{"monday", "tuesday"},
		Enabled:          true,
	}
	testCampaign2 = app.AdvertisingCampaign{
		Name:             "test2",
		DefaultDiscount:  50,
		DiscountPrograms: []app.DiscountProgram{{Discount: 3, ProgramID: 3}},
		StartDate:        time.Date(2024, 3, 10, 0, 0, 0, 0, time.UTC),
		EndDate:          time.Date(2024, 7, 20, 0, 0, 0, 0, time.UTC),
		StartMinute:      10,
		EndMinute:        50,
		Weekday:          []string{"wednesday"},
		Enabled:          true,
	}
	testCampaign3 = app.AdvertisingCampaign{
		Name:             "test3",
		DefaultDiscount:  20,
		DiscountPrograms: []app.DiscountProgram{{Discount: 3, ProgramID: 3}},
		StartDate:        time.Date(2024, 7, 21, 0, 0, 0, 0, time.UTC),
		EndDate:          time.Date(2024, 7, 25, 0, 0, 0, 0, time.UTC),
		StartMinute:      30,
		EndMinute:        40,
		Weekday:          []string{"sunday"},
		Enabled:          true,
	}
)

func addTestData(t *check.C) {
	for i := 1; i < 13; i++ {
		err := testRepo.AddStation("Station" + strconv.Itoa(i))
		t.Nil(err)
	}

}

func addCampaigns(t *testing.T) []app.AdvertisingCampaign {
	addedCampaign1, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)
	addedCampaign2, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign2)
	assert.NilError(t, err)
	addedCampaign3, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign3)
	assert.NilError(t, err)

	return []app.AdvertisingCampaign{addedCampaign1, addedCampaign2, addedCampaign3}
}
