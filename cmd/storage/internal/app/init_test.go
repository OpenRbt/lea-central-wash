package app

import (
	"time"

	"github.com/golang/mock/gomock"
	"github.com/powerman/check"
)

var (
	testAdvertistgCampaign1 = AdvertisingCampaign{
		StartMinute:     21 * 60,
		EndMinute:       23 * 60,
		Timezone:        7 * 60,
		DefaultDiscount: 30,
	}
	testAdvertistgCampaign2 = AdvertisingCampaign{
		StartMinute:     23 * 60,
		EndMinute:       3 * 60,
		Timezone:        7 * 60,
		DefaultDiscount: 50,
		DiscountPrograms: []DiscountProgram{
			{
				ProgramID: 1,
				Discount:  5,
			},
			{
				ProgramID: 2,
				Discount:  15,
			},
			{
				ProgramID: 3,
				Discount:  40,
			},
			{
				ProgramID: 4,
				Discount:  60,
			},
		},
	}
	testAdvertistgCampaign3 = AdvertisingCampaign{
		StartMinute:     60,
		EndMinute:       60,
		Timezone:        7 * 60,
		DefaultDiscount: 10,
	}
)

type mocks struct {
	mockRepo    *MockRepo
	mockKasse   *MockKasseSvc
	mockWeather *MockWeatherSvc
	mockHal     *MockHardwareAccessLayer
}

func testNew(t *check.C) (*app, func(), *mocks) {
	ctrl := gomock.NewController(t)
	m := &mocks{
		mockRepo:    NewMockRepo(ctrl),
		mockKasse:   NewMockKasseSvc(ctrl),
		mockWeather: NewMockWeatherSvc(ctrl),
		mockHal:     NewMockHardwareAccessLayer(ctrl),
	}
	// fix me
	m.mockRepo.EXPECT().Stations().Return(nil, nil).AnyTimes()
	m.mockRepo.EXPECT().LastUpdateConfig().Return(2, nil).AnyTimes()
	m.mockRepo.EXPECT().Programs(gomock.Any()).Return(nil, nil).AnyTimes()
	m.mockRepo.EXPECT().AddStation(gomock.Any()).Return(nil).AnyTimes()

	appl := New(m.mockRepo, m.mockKasse, m.mockWeather, m.mockHal).(*app)

	return appl, ctrl.Finish, m
}

func mustParseTime(t string) time.Time {
	date, err := time.Parse(time.RFC3339, t)
	if err != nil {
		panic(err.Error())
	}
	return date
}
