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
	def.InitMetrics("test")
	InitMetrics("test")
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

	testProgram1 = app.Program{
		ID:                         1,
		Price:                      100,
		Name:                       "test",
		PreflightEnabled:           true,
		MotorSpeedPercent:          50,
		PreflightMotorSpeedPercent: 40,
		IsFinishingProgram:         true,
		Relays:                     []app.Relay{{ID: 1, TimeOff: 2, TimeOn: 3}, {ID: 4, TimeOff: 5, TimeOn: 6}},
		PreflightRelays:            []app.Relay{{ID: 7, TimeOff: 8, TimeOn: 9}, {ID: 10, TimeOff: 11, TimeOn: 12}},
	}
	testProgram2 = app.Program{
		ID:                         2,
		Price:                      50,
		Name:                       "tes2",
		PreflightEnabled:           false,
		MotorSpeedPercent:          10,
		PreflightMotorSpeedPercent: 0,
		IsFinishingProgram:         false,
		Relays:                     []app.Relay{{ID: 1, TimeOff: 2, TimeOn: 3}},
	}
	testProgram3 = app.Program{
		ID:                         3,
		Price:                      0,
		Name:                       "test3",
		PreflightEnabled:           false,
		MotorSpeedPercent:          5,
		PreflightMotorSpeedPercent: 10,
		IsFinishingProgram:         true,
		PreflightRelays:            []app.Relay{{ID: 7, TimeOff: 8, TimeOn: 9}, {ID: 10, TimeOff: 11, TimeOn: 12}},
	}

	testUserCreation = app.UserCreation{
		Login:      "login",
		Password:   "password",
		FirstName:  pntr("first name"),
		LastName:   pntr("last name"),
		MiddleName: pntr("middle name"),
		IsAdmin:    pntr(true),
		IsEngineer: pntr(true),
		IsOperator: pntr(true),
	}
	testUser = app.User{
		ID:         1,
		Login:      testUserCreation.Login,
		Password:   testUserCreation.Password,
		FirstName:  *testUserCreation.FirstName,
		LastName:   *testUserCreation.LastName,
		MiddleName: *testUserCreation.MiddleName,
		IsAdmin:    *testUserCreation.IsAdmin,
		IsEngineer: *testUserCreation.IsEngineer,
		IsOperator: *testUserCreation.IsOperator,
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

	testBuildScript = app.SetBuildScript{
		StationID: 1,
		Name:      "1",
		Commands:  []string{"1", "2", "3"},
	}

	testCreateOpenwashingLog = app.OpenwashingLogCreate{
		StationID: 1,
		Text:      "text",
		Type:      nil,
		Level:     app.InfoLogLevel,
	}

	versionID = 1

	testCreateTask = app.CreateTask{
		StationID: 1,
		VersionID: &versionID,
		Type:      app.GetVersionsTaskType,
	}
)

func addTestData(t *check.C) {
	for i := 1; i < 13; i++ {
		err := testRepo.AddStation("Station" + strconv.Itoa(i))
		t.Nil(err)
	}
}

func addPrograms(t *testing.T) []app.Program {
	program1, err := testRepo.SetProgram(ctx, testProgram1)
	assert.NilError(t, err)
	program2, err := testRepo.SetProgram(ctx, testProgram2)
	assert.NilError(t, err)
	program3, err := testRepo.SetProgram(ctx, testProgram3)
	assert.NilError(t, err)

	return []app.Program{program1, program2, program3}
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

func pntr[T any](value T) *T {
	return &value
}
