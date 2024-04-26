package dal

import (
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/sqlxx"
)

func (r *repo) Collections() (collection []app.CollectionReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &collection, sqlCollections, argGetUsers{})
		if err != nil {
			return err
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) CollectionSetSended(id int) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlCollectionSetSended, map[string]interface{}{
			"id": id,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) MoneyReports() (money []app.MngtMoneyReport, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &money, sqlMoney, argGetUsers{})
		if err != nil {
			return err
		}
		return nil
	})
	return //nolint:nakedret
}

func (r *repo) MoneyReportSetSended(id int) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(sqlMoneySetSended, map[string]interface{}{
			"id": id,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) SetStationForce(station app.Station) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err = tx.NamedExec(sqlSetStation, argSetStation{
			ID:           station.ID,
			Name:         station.Name,
			PreflightSec: station.PreflightSec,
			RelayBoard:   station.RelayBoard,
			Deleted:      station.Deleted,
			Version:      station.Version,
			Force:        station.Force,
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) SetStationSended(id app.StationID) (err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err = tx.NamedExec(sqlSetStationSensed, argID{
			ID: int(id),
		})
		return err
	})
	return //nolint:nakedret
}

func (r *repo) StationNotSended() (stations []app.Station, err error) {
	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []rowStation
		err := tx.NamedSelectContext(ctx, &res, sqlStationNotSended, argGetStations{})
		if err != nil {
			return err
		}
		stations = appStations(res)
		return nil
	})
	return //nolint:nakedret
}
