package app

import (
	"time"

	"github.com/powerman/check"
	gomock "go.uber.org/mock/gomock"
)

var (
	testAdvertistgCampaign1 = AdvertisingCampaign{
		StartMinute:     21 * 60,
		EndMinute:       23 * 60,
		DefaultDiscount: 30,
	}
	testAdvertistgCampaign2 = AdvertisingCampaign{
		StartMinute:     23 * 60,
		EndMinute:       3 * 60,
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
		StartMinute:     0,
		EndMinute:       0,
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
	testApp = true
	ctrl := gomock.NewController(t)
	m := &mocks{
		mockRepo:    NewMockRepo(ctrl),
		mockKasse:   NewMockKasseSvc(ctrl),
		mockWeather: NewMockWeatherSvc(ctrl),
		mockHal:     NewMockHardwareAccessLayer(ctrl),
	}

	m.mockRepo.EXPECT().Stations().Return(nil, nil).AnyTimes()
	m.mockRepo.EXPECT().LastUpdateConfig().Return(2, nil).AnyTimes()
	m.mockRepo.EXPECT().Programs(gomock.Any()).Return(nil, nil).AnyTimes()
	m.mockRepo.EXPECT().AddStation(gomock.Any()).Return(nil).AnyTimes()
	m.mockRepo.EXPECT().GetStationConfigInt(ParameterNameVolumeCoef, StationID(1)).Return(StationConfigVar[int64]{Value: 1000}, nil).AnyTimes()
	m.mockRepo.EXPECT().GetConfigInt(parameterNameTimeZone).Return(ConfigInt{
		Value: 420,
	}, nil)
	m.mockRepo.EXPECT().SetConfigIntIfNotExists(gomock.Any()).Return(nil).AnyTimes()

	appl := New(m.mockRepo, m.mockKasse, m.mockWeather, m.mockHal, PostControlConfig{}).(*app)

	return appl, ctrl.Finish, m
}

func mustParseTime(t string) time.Time {
	date, err := time.Parse(time.RFC3339, t)
	if err != nil {
		panic(err.Error())
	}
	return date
}
