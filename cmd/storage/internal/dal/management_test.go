package dal

import (
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/google/go-cmp/cmp/cmpopts"
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
			name: "successful set",
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
				Version:                    0,
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
				Version:                    0,
			},
		},
		{
			name: "should update program",
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
				Version:                    1,
			},
		},
		{
			name: "should update program with the same version and lower",
			program: app.ManagementProgram{
				ID:      1,
				Name:    "Test",
				Version: 0,
				Force:   true,
			},
			createdProgram: app.Program{
				ID:      1,
				Name:    "Test",
				Version: 0,
			},
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
		err := testRepo.SetProgram(program)
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

func TestUpdateAdvertisingCampaignFromManagement(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	tests := []struct {
		name          string
		campaign      app.ManagementAdvertisingCampaign
		addedCampaign app.AdvertisingCampaign
		expectedError error
	}{
		{
			name: "successfully update",
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
			name: "force update",
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
				Version:          0,
				Force:            true,
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
				Version:          0,
			},
		},
		{
			name: "update and up version",
			campaign: app.ManagementAdvertisingCampaign{
				ID:               1,
				Name:             "updated",
				DefaultDiscount:  10,
				DiscountPrograms: []app.DiscountProgram{{Discount: 1, ProgramID: 1}, {Discount: 2, ProgramID: 2}},
				StartDate:        time.Date(2024, 3, 10, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 3, 15, 0, 0, 0, 0, time.UTC),
				StartMinute:      20,
				EndMinute:        30,
				Weekday:          []string{"wednesday", "friday"},
				Enabled:          true,
				Version:          0,
			},
			addedCampaign: app.AdvertisingCampaign{
				ID:               1,
				Name:             "updated",
				DefaultDiscount:  10,
				DiscountPrograms: []app.DiscountProgram{{Discount: 1, ProgramID: 1}, {Discount: 2, ProgramID: 2}},
				StartDate:        time.Date(2024, 3, 10, 0, 0, 0, 0, time.UTC),
				EndDate:          time.Date(2024, 3, 15, 0, 0, 0, 0, time.UTC),
				StartMinute:      20,
				EndMinute:        30,
				Weekday:          []string{"wednesday", "friday"},
				Enabled:          true,
				Version:          1,
			},
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
