package dal

import (
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/go-openapi/swag"
	"github.com/google/go-cmp/cmp/cmpopts"
	"gotest.tools/v3/assert"
)

func TestGetPrograms(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	addedPrograms := addPrograms(t)

	tests := []struct {
		name          string
		programs      []app.Program
		filter        app.ProgramFilter
		total         int64
		expectedError error
	}{
		{
			name:     "successfully get all",
			programs: addedPrograms,
			total:    3,
		},
		{
			name: "filter by id",
			filter: app.ProgramFilter{
				ID: &addedPrograms[0].ID,
			},
			programs: addedPrograms[0:1],
			total:    1,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			programs, total, err := testRepo.GetPrograms(ctx, tc.filter)
			assert.ErrorIs(t, err, tc.expectedError)
			assert.Equal(t, total, tc.total)
			assert.DeepEqual(t, programs, tc.programs, cmpopts.SortSlices(func(i, j app.Program) bool {
				return i.ID < j.ID
			}))
		})
	}
}

func TestAddAdvertisingCampaign(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	addedCampaign, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)

	assert.DeepEqual(t, testCampaign1, addedCampaign, cmpopts.IgnoreFields(app.AdvertisingCampaign{}, "ID"))
	assert.Assert(t, addedCampaign.ID > 0)
}

func TestEditAdvertisingCampaign(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	addedCampaign, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)

	t.Run("successfully edit", func(t *testing.T) {
		campaign := app.AdvertisingCampaign{
			ID:               addedCampaign.ID,
			Name:             "new",
			DefaultDiscount:  5,
			DiscountPrograms: []app.DiscountProgram{{Discount: 3, ProgramID: 3}},
			StartDate:        time.Date(2024, 6, 20, 0, 0, 0, 0, time.UTC),
			EndDate:          time.Date(2024, 6, 25, 0, 0, 0, 0, time.UTC),
			StartMinute:      10,
			EndMinute:        50,
			Weekday:          []string{"wednesday"},
			Enabled:          false,
			Version:          1,
		}

		editedCampaign, err := testRepo.EditAdvertisingCampaign(ctx, campaign)

		assert.NilError(t, err)
		assert.DeepEqual(t, editedCampaign, campaign)
	})
}

func TestDeleteAdvertisingCampaign(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	addedCampaign, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)

	err = testRepo.DeleteAdvertisingCampaign(ctx, addedCampaign.ID)
	assert.NilError(t, err)

	_, err = testRepo.GetAdvertisingCampaignByID(ctx, addedCampaign.ID)
	assert.ErrorIs(t, err, app.ErrNotFound)
}

func TestGetAdvertisingCampaignByID(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	addedCampaign, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)

	tests := []struct {
		name          string
		id            int64
		campaign      app.AdvertisingCampaign
		expectedError error
	}{
		{
			name: "successfully get",
			id:   addedCampaign.ID,
			campaign: (func() app.AdvertisingCampaign {
				advert := testCampaign1
				advert.ID = addedCampaign.ID
				return advert
			})(),
		},
		{
			name:          "error when campaign not exist",
			id:            999,
			expectedError: app.ErrNotFound,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			campaign, err := testRepo.GetAdvertisingCampaignByID(ctx, tc.id)
			assert.ErrorIs(t, err, tc.expectedError)
			if err == nil {
				assert.DeepEqual(t, campaign, tc.campaign)
			}
		})
	}
}

func TestGetAdvertisingCampaigns(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	addedCampaigns := addCampaigns(t)

	tests := []struct {
		name          string
		campaigns     []app.AdvertisingCampaign
		filter        app.AdvertisingCampaignFilter
		total         int64
		expectedError error
	}{
		{
			name:      "successfully get all",
			campaigns: addedCampaigns,
			total:     3,
		},
		{
			name: "filter start date",
			filter: app.AdvertisingCampaignFilter{
				StartDate: swag.Time(time.Date(2024, 7, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[1:3],
			total:     2,
		},
		{
			name: "filter end date",
			filter: app.AdvertisingCampaignFilter{
				EndDate: swag.Time(time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[:2],
			total:     2,
		},
		{
			name: "filter start and end dates",
			filter: app.AdvertisingCampaignFilter{
				StartDate: swag.Time(time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC)),
				EndDate:   swag.Time(time.Date(2024, 7, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[:2],
			total:     2,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			campaigns, total, err := testRepo.GetAdvertisingCampaigns(ctx, tc.filter)
			assert.ErrorIs(t, err, tc.expectedError)
			assert.Equal(t, total, tc.total)
			assert.DeepEqual(t, campaigns, tc.campaigns, cmpopts.SortSlices(func(i, j app.AdvertisingCampaign) bool {
				return i.ID < j.ID
			}))
		})
	}
}

func TestGetCurrentAdvertisingCampaigns(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	addedCampaigns := addCampaigns(t)

	tests := []struct {
		name          string
		campaigns     []app.AdvertisingCampaign
		currentTime   time.Time
		expectedError error
	}{
		{
			name:        "current time in middle",
			currentTime: time.Date(2024, 5, 21, 0, 0, 0, 0, time.UTC),
			campaigns:   addedCampaigns[:2],
		},
		{
			name:        "current time in left boundary",
			currentTime: time.Date(2024, 3, 10, 0, 0, 0, 0, time.UTC),
			campaigns:   addedCampaigns[1:2],
		},
		{
			name:        "current time in right boundary",
			currentTime: time.Date(2024, 7, 25, 0, 0, 0, 0, time.UTC),
			campaigns:   addedCampaigns[2:3],
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			campaigns, err := testRepo.GetCurrentAdvertisingCampaigns(tc.currentTime)
			assert.ErrorIs(t, err, tc.expectedError)
			assert.DeepEqual(t, campaigns, tc.campaigns, cmpopts.SortSlices(func(i, j app.AdvertisingCampaign) bool {
				return i.ID < j.ID
			}))
		})
	}
}
