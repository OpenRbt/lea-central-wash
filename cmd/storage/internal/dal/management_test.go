package dal

import (
	"testing"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/check"
)

func TestStation(tt *testing.T) {
	t := check.T(tt)
	defer t.Nil(testRepo.truncate())
	addTestData(t)

	for i := 1; i < 13; i++ {
		err := testRepo.SetStationSended(app.StationID(i))
		t.Nil(err)
	}

	s, err := testRepo.StationNotSended()
	t.Nil(err)
	t.Nil(s)
	station2, err := testRepo.Station(app.StationID(2))
	t.Nil(err)
	err = testRepo.SetStation(station2)
	t.Nil(err)

	station := app.Station{
		ID:           station2.ID,
		Name:         station2.Name,
		PreflightSec: station2.PreflightSec,
		RelayBoard:   station2.RelayBoard,
		Deleted:      false,
		Version:      1,
	}
	s, err = testRepo.StationNotSended()
	t.Nil(err)
	t.DeepEqual(s, []app.Station{station})

	station.Name = "up name"
	err = testRepo.SetStationForce(station)
	t.Nil(err)

	stationUp, err := testRepo.Station(app.StationID(2))
	t.Nil(err)
	t.DeepEqual(stationUp, station2)

	station2.Name = "up name"
	station.Version++
	err = testRepo.SetStationForce(station)
	t.Nil(err)

	stationUp, err = testRepo.Station(app.StationID(2))
	t.Nil(err)
	t.DeepEqual(stationUp, station2)

	station.Name = "up2 name"
	station2.Name = "up2 name"
	station.Force = true
	err = testRepo.SetStationForce(station)
	t.Nil(err)

	stationUp, err = testRepo.Station(app.StationID(2))
	t.Nil(err)
	t.DeepEqual(stationUp, station2)

	s, err = testRepo.StationNotSended()
	t.Nil(err)
	t.Nil(s)

	err = testRepo.DelStation(station.ID)
	t.Nil(err)
	station.Deleted = true
	station.Version++
	station.Force = false
	s, err = testRepo.StationNotSended()
	t.Nil(err)
	t.DeepEqual(s, []app.Station{station})

}
