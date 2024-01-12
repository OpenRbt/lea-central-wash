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
