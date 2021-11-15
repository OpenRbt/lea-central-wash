package app

import (
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/powerman/check"
)

func TestAddAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

	//mockRepo := mocks.NewMockRepo(ctrl)
	//mockKasse := mocks.NewMockKasseSvc(ctrl)
	//mockWeather := mocks.NewMockWeatherSvc(ctrl)
	//mockHal := mocks.NewMockHardwareAccessLayer(ctrl)

	//a := app{
	//	repo:       mockRepo,
	//	kasseSvc:   mockKasse,
	//	weatherSvc: mockWeather,
	//	hardware:   mockHal,
	//}

	//mockRepo.EXPECT().AddAdvertisingCampaign(gomock.Any()).Return(nil)
	//res, err := a.AddAdvertisingCampaign(nil, AdvertisingCampaign{})

	//t.Nil(err)
	//t.DeepEqual(testAdvertistgCampagin1, res)
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

func TestCheckDiscounts(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
