package app

import (
	"github.com/golang/mock/gomock"
	"github.com/powerman/check"
)

var (
	testAdvertistgCampagin1 = AdvertisingCampaign{}
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
