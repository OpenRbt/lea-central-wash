package app

import (
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/powerman/check"
)

func TestAddAdvertisingCampaign(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()

	a, finish, mocks := testNew(t)
	defer finish()

	mocks.mockRepo.EXPECT().AddAdvertisingCampaign(gomock.Any()).Return(nil)

	err := a.AddAdvertisingCampaign(nil, AdvertisingCampaign{})

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

func TestCheckDiscounts(tt *testing.T) {
	t := check.T(tt)
	t.Parallel()
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

}
