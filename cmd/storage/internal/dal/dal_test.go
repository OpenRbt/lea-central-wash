package dal

import (
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/go-openapi/swag"
	"github.com/google/go-cmp/cmp/cmpopts"
	"gotest.tools/v3/assert"
)

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
		editedCampaign := app.AdvertisingCampaign{
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

		err := testRepo.EditAdvertisingCampaign(editedCampaign)
		assert.NilError(t, err)

		campaign, err := testRepo.AdvertisingCampaignByID(editedCampaign.ID)
		assert.NilError(t, err)
		assert.DeepEqual(t, *campaign, editedCampaign)
	})
}

func TestDelAdvertisingCampaign(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	addedCampaign, err := testRepo.AddAdvertisingCampaign(ctx, testCampaign1)
	assert.NilError(t, err)

	err = testRepo.DelAdvertisingCampaign(addedCampaign.ID)
	assert.NilError(t, err)

	_, err = testRepo.AdvertisingCampaignByID(addedCampaign.ID)
	assert.ErrorIs(t, err, app.ErrNotFound)
}

func TestAdvertisingCampaignByID(t *testing.T) {
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
			campaign, err := testRepo.AdvertisingCampaignByID(tc.id)
			assert.ErrorIs(t, err, tc.expectedError)
			if err == nil {
				assert.DeepEqual(t, *campaign, tc.campaign)
			}
		})
	}
}

func TestAdvertisingCampaign(t *testing.T) {
	assert.NilError(t, testRepo.truncate())
	addedCampaigns := addCampaigns(t)

	tests := []struct {
		name      string
		campaigns []app.AdvertisingCampaign
		filter    struct {
			startDate *time.Time
			endDate   *time.Time
		}
		expectedError error
	}{
		{
			name:      "successfully get all",
			campaigns: addedCampaigns,
		},
		{
			name: "filter start date",
			filter: struct {
				startDate *time.Time
				endDate   *time.Time
			}{
				startDate: swag.Time(time.Date(2024, 7, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[1:3],
		},
		{
			name: "filter end date",
			filter: struct {
				startDate *time.Time
				endDate   *time.Time
			}{
				endDate: swag.Time(time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[:2],
		},
		{
			name: "filter start and end dates",
			filter: struct {
				startDate *time.Time
				endDate   *time.Time
			}{
				startDate: swag.Time(time.Date(2024, 5, 20, 0, 0, 0, 0, time.UTC)),
				endDate:   swag.Time(time.Date(2024, 7, 20, 0, 0, 0, 0, time.UTC)),
			},
			campaigns: addedCampaigns[:2],
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			campaigns, err := testRepo.AdvertisingCampaign(tc.filter.startDate, tc.filter.endDate)
			assert.ErrorIs(t, err, tc.expectedError)
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
