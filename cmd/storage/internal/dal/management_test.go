package dal

import (
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/powerman/check"
	"gotest.tools/v3/assert"
)

func TestSetProgramFromManagement(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	tests := []struct {
		name           string
		program        app.ManagementProgram
		createdProgram app.Program
		expectedError  error
	}{
		{
			name: "successful insert",
			program: app.ManagementProgram{
				ID:                         1,
				Price:                      10,
				Name:                       "Test",
				PreflightEnabled:           true,
				MotorSpeedPercent:          70,
				PreflightMotorSpeedPercent: 70,
				Relays:                     []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}, {ID: 2, TimeOn: 3, TimeOff: 4}},
				PreflightRelays:            []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}, {ID: 2, TimeOn: 3, TimeOff: 4}},
				IsFinishingProgram:         true,
				Version:                    1,
			},
			createdProgram: app.Program{
				ID:                         1,
				Price:                      10,
				Name:                       "Test",
				PreflightEnabled:           true,
				MotorSpeedPercent:          70,
				PreflightMotorSpeedPercent: 70,
				Relays:                     []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}, {ID: 2, TimeOn: 3, TimeOff: 4}},
				PreflightRelays:            []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}, {ID: 2, TimeOn: 3, TimeOff: 4}},
				IsFinishingProgram:         true,
				Version:                    1,
			},
		},
		{
			name: "should update program with higher version",
			program: app.ManagementProgram{
				ID:                         1,
				Price:                      20,
				Name:                       "New",
				PreflightEnabled:           false,
				MotorSpeedPercent:          40,
				PreflightMotorSpeedPercent: 40,
				Relays:                     []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}},
				PreflightRelays:            []app.Relay{{ID: 2, TimeOn: 3, TimeOff: 4}},
				IsFinishingProgram:         false,
				Version:                    3,
			},
			createdProgram: app.Program{
				ID:                         1,
				Price:                      20,
				Name:                       "New",
				PreflightEnabled:           false,
				MotorSpeedPercent:          40,
				PreflightMotorSpeedPercent: 40,
				Relays:                     []app.Relay{{ID: 1, TimeOn: 1, TimeOff: 2}},
				PreflightRelays:            []app.Relay{{ID: 2, TimeOn: 3, TimeOff: 4}},
				IsFinishingProgram:         false,
				Version:                    3,
			},
		},
		{
			name: "should forcefully update program with the same or lower version",
			program: app.ManagementProgram{
				ID:      1,
				Name:    "Test",
				Version: 2,
				Force:   true,
			},
			createdProgram: app.Program{
				ID:      1,
				Name:    "Test",
				Version: 2,
			},
		},
		{
			name: "error when trying to update with the same or lower version",
			program: app.ManagementProgram{
				ID:      1,
				Name:    "Test",
				Version: 0,
			},
			expectedError: app.ErrSameOrLowerVersion,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			program, err := testRepo.SetProgramFromManagement(ctx, tc.program)
			assert.ErrorIs(t, err, tc.expectedError)
			assert.DeepEqual(t, tc.createdProgram, program)
		})
	}
}

func TestNotSendedPrograms(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	programs := []app.Program{
		{ID: 1, Name: "1"},
		{ID: 2, Name: "2"},
		{ID: 3, Name: "3"},
	}

	for _, program := range programs {
		_, err := testRepo.SetProgram(ctx, program)
		assert.NilError(t, err)
	}

	err := testRepo.MarkProgramSended(ctx, programs[2].ID)
	assert.NilError(t, err)

	notSendedPrograms, err := testRepo.NotSendedPrograms(ctx)
	assert.NilError(t, err)

	assert.DeepEqual(t, notSendedPrograms, programs[:2], cmpopts.SortSlices(func(i, j app.Program) bool {
		return i.ID > j.ID
	}))
}

func TestUpsertAdvertisingCampaignFromManagement(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	tests := []struct {
		name          string
		campaign      app.ManagementAdvertisingCampaign
		addedCampaign app.AdvertisingCampaign
		expectedError error
	}{
		{
			name: "successfully insert",
			campaign: app.ManagementAdvertisingCampaign{
				ID:               1,
				Name:             "test",
				DefaultDiscount:  10,
				DiscountPrograms: []app.DiscountProgram{{Discount: 1, ProgramID: 1}, {Discount: 2, ProgramID: 2}},
				StartDate:        time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 5, 25, 0, 0, 0, 0, time.UTC),
				StartMinute:      0,
				EndMinute:        60,
				Weekday:          []string{"monday", "tuesday"},
				Enabled:          true,
				Version:          1,
			},
			addedCampaign: app.AdvertisingCampaign{
				ID:               1,
				Name:             "test",
				DefaultDiscount:  10,
				DiscountPrograms: []app.DiscountProgram{{Discount: 1, ProgramID: 1}, {Discount: 2, ProgramID: 2}},
				StartDate:        time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 5, 25, 0, 0, 0, 0, time.UTC),
				StartMinute:      0,
				EndMinute:        60,
				Weekday:          []string{"monday", "tuesday"},
				Enabled:          true,
				Version:          1,
			},
		},
		{
			name: "should update campaign with higher version",
			campaign: app.ManagementAdvertisingCampaign{
				ID:               1,
				Name:             "new",
				DefaultDiscount:  5,
				DiscountPrograms: []app.DiscountProgram{{Discount: 3, ProgramID: 3}},
				StartDate:        time.Date(2024, 6, 20, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 6, 25, 0, 0, 0, 0, time.UTC),
				StartMinute:      10,
				EndMinute:        50,
				Weekday:          []string{"wednesday"},
				Enabled:          false,
				Version:          3,
			},
			addedCampaign: app.AdvertisingCampaign{
				ID:               1,
				Name:             "new",
				DefaultDiscount:  5,
				DiscountPrograms: []app.DiscountProgram{{Discount: 3, ProgramID: 3}},
				StartDate:        time.Date(2024, 6, 20, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 6, 25, 0, 0, 0, 0, time.UTC),
				StartMinute:      10,
				EndMinute:        50,
				Weekday:          []string{"wednesday"},
				Enabled:          false,
				Version:          3,
			},
		},
		{
			name: "force update",
			campaign: app.ManagementAdvertisingCampaign{
				ID:      1,
				Name:    "force",
				Version: 1,
				Force:   true,
			},
			addedCampaign: app.AdvertisingCampaign{
				ID:      1,
				Weekday: []string{},
				Name:    "force",
				Version: 1,
			},
		},
		{
			name: "error when trying to update with the same or lower version",
			campaign: app.ManagementAdvertisingCampaign{
				ID:      1,
				Name:    "lower",
				Version: 0,
			},
			expectedError: app.ErrSameOrLowerVersion,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			campaign, err := testRepo.UpsertAdvertisingCampaignFromManagement(ctx, tc.campaign)
			assert.ErrorIs(t, err, tc.expectedError)
			assert.DeepEqual(t, campaign, tc.addedCampaign)
		})
	}
}

func TestNotSendedAdvertisingCampaigns(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	campaigns := []app.AdvertisingCampaign{
		{ID: 1, Name: "1", Weekday: []string{}},
		{ID: 2, Name: "2", Weekday: []string{}},
		{ID: 3, Name: "3", Weekday: []string{}},
	}

	for _, campaign := range campaigns {
		_, err := testRepo.AddAdvertisingCampaign(ctx, campaign)
		assert.NilError(t, err)
	}

	err := testRepo.MarkAdvertisingCampaignSended(ctx, campaigns[2].ID)
	assert.NilError(t, err)

	notSendedCampaigns, err := testRepo.NotSendedAdvertisingCampaigns(ctx)
	assert.NilError(t, err)

	assert.DeepEqual(t, notSendedCampaigns, campaigns[:2], cmpopts.SortSlices(func(i, j app.AdvertisingCampaign) bool {
		return i.ID > j.ID
	}))
}

func TestNotSendedConfigInts(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	configs := []app.ConfigInt{
		{Name: "NAME1", Value: 1, Description: "description1", Note: "note1"},
		{Name: "NAME2", Value: 2, Description: "description2", Note: "note2"},
		{Name: "NAME3", Value: 3, Description: "description3", Note: "note3"},
	}

	for _, config := range configs {
		err := testRepo.SetConfigInt(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkConfigIntSended(ctx, configs[2].Name)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedConfigInts(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetConfigInt(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedConfigInts(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedConfigBools(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	configs := []app.ConfigBool{
		{Name: "NAME1", Value: true, Description: "description1", Note: "note1"},
		{Name: "NAME2", Value: false, Description: "description2", Note: "note2"},
		{Name: "NAME3", Value: true, Description: "description3", Note: "note3"},
	}

	for _, config := range configs {
		err := testRepo.SetConfigBool(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkConfigBoolSended(ctx, configs[2].Name)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedConfigBools(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetConfigBool(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedConfigBools(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedConfigStrings(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	configs := []app.ConfigString{
		{Name: "NAME1", Value: "value1", Description: "description1", Note: "note1"},
		{Name: "NAME2", Value: "value2", Description: "description2", Note: "note2"},
		{Name: "NAME3", Value: "value3", Description: "description3", Note: "note3"},
	}

	for _, config := range configs {
		err := testRepo.SetConfigString(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkConfigStringSended(ctx, configs[2].Name)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedConfigStrings(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetConfigString(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedConfigStrings(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)

	err = testRepo.DeleteConfigString(configs[2].Name)
	assert.NilError(t, err)

	cTemp.Version++
	cTemp.Deleted = true
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedConfigStrings(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedStationConfigBools(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	tt := check.T(t)
	addTestData(tt)

	configs := []app.StationConfigVar[bool]{
		{Name: "NAME1", Value: true, Description: "description1", Note: "note1", StationID: 1},
		{Name: "NAME2", Value: false, Description: "description2", Note: "note2", StationID: 1},
		{Name: "NAME3", Value: true, Description: "description3", Note: "note3", StationID: 1},
	}

	for _, config := range configs {
		err := testRepo.SetStationConfigBool(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkStationConfigBoolSended(ctx, configs[2].Name, configs[2].StationID)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedStationConfigBools(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetStationConfigBool(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedStationConfigBools(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedStationConfigInts(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	tt := check.T(t)
	addTestData(tt)

	configs := []app.StationConfigVar[int64]{
		{Name: "NAME1", Value: 1, Description: "description1", Note: "note1", StationID: 1},
		{Name: "NAME2", Value: 2, Description: "description2", Note: "note2", StationID: 1},
		{Name: "NAME3", Value: 3, Description: "description3", Note: "note3", StationID: 1},
	}

	for _, config := range configs {
		err := testRepo.SetStationConfigInt(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkStationConfigIntSended(ctx, configs[2].Name, configs[2].StationID)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedStationConfigInts(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetStationConfigInt(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedStationConfigInts(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedStationConfigStrings(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	tt := check.T(t)
	addTestData(tt)

	configs := []app.StationConfigVar[string]{
		{Name: "NAME1", Value: "value1", Description: "description1", Note: "note1", StationID: 1},
		{Name: "NAME2", Value: "value2", Description: "description2", Note: "note2", StationID: 1},
		{Name: "NAME3", Value: "value3", Description: "description3", Note: "note3", StationID: 1},
	}

	for _, config := range configs {
		err := testRepo.SetStationConfigString(config)
		assert.NilError(t, err)
	}

	err := testRepo.MarkStationConfigStringSended(ctx, configs[2].Name, configs[2].StationID)
	assert.NilError(t, err)

	notSendedConfigs, err := testRepo.NotSendedStationConfigStrings(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs[:2])

	err = testRepo.SetStationConfigString(configs[2])
	assert.NilError(t, err)

	cTemp := configs[2]
	cTemp.Version++
	configs[2] = cTemp

	notSendedConfigs, err = testRepo.NotSendedStationConfigStrings(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedConfigs, configs)
}

func TestNotSendedUsers(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	tt := check.T(t)
	addTestData(tt)

	createUsers := []app.UserCreation{
		{Login: "login1", Password: "password", FirstName: pntr("first name"), MiddleName: pntr("middle name"), LastName: pntr("last name"), IsAdmin: pntr(true), IsEngineer: pntr(true), IsOperator: pntr(true)},
		{Login: "login2", Password: "password", FirstName: pntr("first name"), MiddleName: pntr("middle name"), LastName: pntr("last name"), IsAdmin: pntr(true), IsEngineer: pntr(true), IsOperator: pntr(true)},
		{Login: "login3", Password: "password", FirstName: pntr("first name"), MiddleName: pntr("middle name"), LastName: pntr("last name"), IsAdmin: pntr(true), IsEngineer: pntr(true), IsOperator: pntr(true)},
	}
	users := []app.User{}

	for _, user := range createUsers {
		user, err := testRepo.CreateUser(user)
		assert.NilError(t, err)

		users = append(users, user)
	}

	err := testRepo.MarkUserSended(ctx, users[2].Login)
	assert.NilError(t, err)

	notSendedUsers, err := testRepo.NotSendedUsers(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedUsers, users[:2])

	newFirstName := "new first name"
	_, err = testRepo.UpdateUser(users[2].Login, app.UserUpdate{FirstName: &newFirstName})
	assert.NilError(t, err)

	cTemp := users[2]
	cTemp.Version++
	cTemp.FirstName = newFirstName
	users[2] = cTemp

	notSendedUsers, err = testRepo.NotSendedUsers(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedUsers, users)

	_, err = testRepo.DeleteUser(ctx, users[2].Login)
	assert.NilError(t, err)

	cTemp = users[2]
	cTemp.Version++
	cTemp.Deleted = true
	users[2] = cTemp

	notSendedUsers, err = testRepo.NotSendedUsers(ctx)
	assert.NilError(t, err)
	assert.DeepEqual(t, notSendedUsers, users)
}
