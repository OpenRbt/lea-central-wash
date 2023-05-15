package app

import (
	"testing"
	"time"

	"github.com/golang/mock/gomock"
	"github.com/powerman/check"
)

func TestIsValidPromotion(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	testCases := []struct {
		localTime time.Time
		a         AdvertisingCampaign
		want      bool
	}{
		// time
		{mustParseTime("2021-11-01T02:00:00Z"), AdvertisingCampaign{
			StartMinute: 0,
			EndMinute:   0,
		}, true},
		{mustParseTime("2021-11-01T02:00:00Z"), AdvertisingCampaign{
			StartMinute: 2 * 60,
			EndMinute:   2 * 60,
		}, false},
		{mustParseTime("2021-11-01T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 1 * 60,
			EndMinute:   2 * 60,
		}, true},
		{mustParseTime("2021-11-01T01:00:00Z"), AdvertisingCampaign{
			StartMinute: 1 * 60,
			EndMinute:   2 * 60,
		}, true},
		{mustParseTime("2021-11-01T02:00:00Z"), AdvertisingCampaign{
			StartMinute: 1 * 60,
			EndMinute:   2 * 60,
		}, false},
		{mustParseTime("2021-11-01T23:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
		}, true},
		{mustParseTime("2021-11-02T00:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
		}, true},
		{mustParseTime("2021-11-01T23:00:00Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
		}, true},
		{mustParseTime("2021-11-02T02:00:00Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
		}, false},
		{mustParseTime("2021-11-02T02:00:00Z"), AdvertisingCampaign{
			StartMinute: 0,
			EndMinute:   0,
		}, true},
		// weekday
		{mustParseTime("2021-11-01T23:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"monday"},
		}, true},
		{mustParseTime("2021-11-02T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"monday"},
		}, true},
		{mustParseTime("2021-11-01T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 1 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"monday"},
		}, true},
		{mustParseTime("2021-11-01T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"monday"},
		}, false},
		{mustParseTime("2021-11-01T23:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"sunday"},
		}, false},
		{mustParseTime("2021-11-02T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 23 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"tuesday"},
		}, false},
		{mustParseTime("2021-11-01T01:22:46Z"), AdvertisingCampaign{
			StartMinute: 1 * 60,
			EndMinute:   2 * 60,
			Weekday:     []string{"wednesday"},
		}, false},
	}
	for _, tc := range testCases {
		tc := tc
		t.Run("", func(tt *testing.T) {
			t := check.T(tt)
			res := isValidPromotion(tc.localTime, tc.a)
			t.Equal(res, tc.want)
		})
	}

}

func TestCurrentAdvertisingCampaigns(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	a, finish, mocks := testNew(t)
	defer finish()

	mocks.mockRepo.EXPECT().GetCurrentAdvertisingCampaigns(gomock.Any()).Return([]AdvertisingCampaign{
		testAdvertistgCampaign1,
		testAdvertistgCampaign2,
		testAdvertistgCampaign3,
	}, nil).Times(2)

	getted, err := a.currentAdvertisingCampaigns(mustParseTime("2021-11-02T01:22:46Z"))
	t.Nil(err)
	t.DeepEqual(getted, []AdvertisingCampaign{
		testAdvertistgCampaign2,
		testAdvertistgCampaign3,
	})
	getted, err = a.currentAdvertisingCampaigns(mustParseTime("2021-11-01T21:22:46Z"))
	t.Nil(err)
	t.DeepEqual(getted, []AdvertisingCampaign{
		testAdvertistgCampaign1,
		testAdvertistgCampaign3,
	})
}

func TestCheckDiscounts(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	a, finish, mocks := testNew(t)
	defer finish()

	mocks.mockRepo.EXPECT().GetCurrentAdvertisingCampaigns(gomock.Any()).Return([]AdvertisingCampaign{
		testAdvertistgCampaign1,
		testAdvertistgCampaign2,
		testAdvertistgCampaign3,
	}, nil).Times(2)
	curTime := mustParseTime("2021-11-02T01:22:46Z")
	err := a.checkDiscounts(curTime)
	t.Nil(err)
	t.DeepEqual(a.lastDiscountUpdate, curTime.Unix())
	err = a.checkDiscounts(mustParseTime("2021-11-02T01:22:46Z"))
	t.Nil(err)
	t.DeepEqual(a.lastDiscountUpdate, curTime.Unix())
	t.DeepEqual(a.programsDiscounts, ProgramsDiscount{
		DefaultDiscount: 50,
		Discounts: map[int64]int64{
			1: 10,
			2: 15,
			3: 40,
			4: 60,
		},
	})
	mocks.mockRepo.EXPECT().GetCurrentAdvertisingCampaigns(gomock.Any()).Return([]AdvertisingCampaign{
		testAdvertistgCampaign3,
		testAdvertistgCampaign2,
		testAdvertistgCampaign1,
	}, nil)
	err = a.checkDiscounts(mustParseTime("2021-11-02T01:23:46Z"))
	t.Nil(err)
	t.DeepEqual(a.lastDiscountUpdate, curTime.Unix())
	t.DeepEqual(a.programsDiscounts, ProgramsDiscount{
		DefaultDiscount: 50,
		Discounts: map[int64]int64{
			1: 10,
			2: 15,
			3: 40,
			4: 60,
		},
	})

}

func TestAddAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()

	a, finish, mocks := testNew(t)
	defer finish()

	mocks.mockRepo.EXPECT().AddAdvertisingCampaign(gomock.Any()).Return(nil)

	err := a.AddAdvertisingCampaign(&Auth{IsAdmin: true}, AdvertisingCampaign{})

	t.Nil(err)
}
func TestEditAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
func TestDelAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
func TestAdvertisingCampaignByID(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
func TestAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
