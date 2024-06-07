package dal

import (
	"fmt"
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

	campaign, err := testRepo.DeleteAdvertisingCampaign(ctx, addedCampaign.ID)
	assert.NilError(t, err)
	assert.Equal(t, campaign.Version, addedCampaign.Version+1)
	assert.Assert(t, campaign.Deleted)

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

func TestCreateUser(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	user, err := testRepo.CreateUser(testUserCreation)
	assert.NilError(t, err)
	assert.DeepEqual(t, user, testUser)

	_, err = testRepo.CreateUser(testUserCreation)
	assert.ErrorIs(t, err, app.ErrLoginNotUnique)
}

func TestGetUser(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	_, err := testRepo.CreateUser(testUserCreation)
	assert.NilError(t, err)

	user, err := testRepo.User(testUserCreation.Login)
	assert.NilError(t, err)
	assert.DeepEqual(t, user, testUser)

	_, err = testRepo.User("non-existent login")
	assert.ErrorIs(t, err, app.ErrNotFound)
}

func TestDeleteUser(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	user, err := testRepo.CreateUser(testUserCreation)
	assert.NilError(t, err)

	user.Deleted = true
	user.Version++

	deletedUser, err := testRepo.DeleteUser(ctx, user.Login)
	assert.NilError(t, err)
	assert.DeepEqual(t, deletedUser, user)

	_, err = testRepo.User(user.Login)
	assert.ErrorIs(t, err, app.ErrNotFound)
}

func TestUserList(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	users := []app.User{
		testUser,
		testUser,
		testUser,
	}

	for i, v := range users {
		userCreation := testUserCreation
		v.Login += fmt.Sprintf("%d", i)
		v.Password += fmt.Sprintf("%d", i)
		v.ID = i + 1
		userCreation.Login = v.Login
		userCreation.Password = v.Password
		users[i] = v

		_, err := testRepo.CreateUser(userCreation)
		assert.NilError(t, err)
	}

	list, _, err := testRepo.Users(ctx, app.UserFilter{})
	assert.NilError(t, err)
	assert.DeepEqual(t, list, users, cmpopts.SortSlices(func(i, j app.User) bool {
		if i.LastName != j.LastName {
			return i.LastName < j.LastName
		} else if i.FirstName != j.FirstName {
			return i.FirstName < j.FirstName
		} else {
			return i.MiddleName < j.MiddleName
		}
	}))

	_, err = testRepo.UpdateUser(users[2].Login, app.UserUpdate{IsAdmin: pntr(false)})
	assert.NilError(t, err)

	list, _, err = testRepo.Users(ctx, app.UserFilter{IsAdmin: pntr(true)})
	assert.NilError(t, err)
	assert.DeepEqual(t, list, users[:2], cmpopts.SortSlices(func(i, j app.User) bool {
		if i.LastName != j.LastName {
			return i.LastName < j.LastName
		} else if i.FirstName != j.FirstName {
			return i.FirstName < j.FirstName
		} else {
			return i.MiddleName < j.MiddleName
		}
	}))

	_, err = testRepo.DeleteUser(ctx, users[2].Login)
	assert.NilError(t, err)

	list, _, err = testRepo.Users(ctx, app.UserFilter{})
	assert.NilError(t, err)
	assert.DeepEqual(t, list, users[:2], cmpopts.SortSlices(func(i, j app.User) bool {
		if i.LastName != j.LastName {
			return i.LastName < j.LastName
		} else if i.FirstName != j.FirstName {
			return i.FirstName < j.FirstName
		} else {
			return i.MiddleName < j.MiddleName
		}
	}))

}

func TestUpdateUser(t *testing.T) {
	assert.NilError(t, testRepo.truncate())

	user, err := testRepo.CreateUser(testUserCreation)
	assert.NilError(t, err)

	userUpdate := app.UserUpdate{
		FirstName:  pntr("new first name"),
		IsOperator: pntr(false),
	}
	user.FirstName = *userUpdate.FirstName
	user.IsOperator = *userUpdate.IsOperator
	user.Version++

	updatedUser, err := testRepo.UpdateUser(user.Login, userUpdate)
	assert.NilError(t, err)
	assert.DeepEqual(t, user, updatedUser)

	_, err = testRepo.UpdateUser("non-existent login", userUpdate)
	assert.ErrorIs(t, err, app.ErrNotFound)
}
