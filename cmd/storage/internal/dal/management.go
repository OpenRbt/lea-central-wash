package dal

import (
	"context"
	"database/sql"
	"errors"
	"strings"

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

func (r *repo) SetProgramFromManagement(ctx context.Context, program app.ManagementProgram) (app.Program, error) {
	var rowProgram resProgram
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &rowProgram, sqlSetProgramFromManagement, argManagementProgram{
			ID:                         program.ID,
			Price:                      program.Price,
			Name:                       program.Name,
			PreflightEnabled:           program.PreflightEnabled,
			Relays:                     dalProgramRelays(program.Relays),
			PreflightRelays:            dalProgramRelays(program.PreflightRelays),
			MotorSpeedPercent:          program.MotorSpeedPercent,
			PreflightMotorSpeedPercent: program.PreflightMotorSpeedPercent,
			IsFinishingProgram:         program.IsFinishingProgram,
			Version:                    program.Version,
			Force:                      program.Force,
		})
		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrSameOrLowerVersion
		}

		return err
	})
	if err != nil {
		return app.Program{}, err
	}

	return appProgram(rowProgram), nil
}

func (r *repo) NotSendedPrograms(ctx context.Context) ([]app.Program, error) {
	var resPrograms []resProgram
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &resPrograms, sqlNotSendedPrograms)

	})
	if err != nil {
		return nil, err
	}

	return appPrograms(resPrograms), nil
}

func (r *repo) MarkProgramSended(ctx context.Context, id int64) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkProgramSended, argID[int64]{
			ID: id,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}

func (r *repo) NotSendedOpenwashingLogs(ctx context.Context) ([]app.OpenwashingLog, error) {
	var respOpenwashingLog []respOpenwashingLog
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &respOpenwashingLog, sqlNotSendedOpenwashingLogs)
	})
	if err != nil {
		return nil, err
	}

	return appOpenwashingLogs(respOpenwashingLog), nil
}

func (r *repo) MarkOpenwashingLogSended(ctx context.Context, id int64) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkOpenwashingLogSended, argID[int64]{
			ID: id,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}

func (r *repo) NotSendedConfigStrings(ctx context.Context) ([]app.ConfigString, error) {
	var respConfigStrings []resGetConfigString
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &respConfigStrings, sqlNotSendedConfigStrings)
	})
	if err != nil {
		return nil, err
	}

	return appConfigStrings(respConfigStrings), nil
}

func (r *repo) MarkConfigStringSended(ctx context.Context, name string) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkConfigStringSended, argID[string]{
			ID: name,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}

func (r *repo) NotSendedConfigInts(ctx context.Context) ([]app.ConfigInt, error) {
	var respConfigInts []resGetConfigInt
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &respConfigInts, sqlNotSendedConfigInts)
	})
	if err != nil {
		return nil, err
	}

	return appConfigInts(respConfigInts), nil
}

func (r *repo) MarkConfigIntSended(ctx context.Context, name string) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkConfigIntSended, argID[string]{
			ID: name,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}

func (r *repo) NotSendedConfigBools(ctx context.Context) ([]app.ConfigBool, error) {
	var respConfigBools []resGetConfigBool
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &respConfigBools, sqlNotSendedConfigBools)
	})
	if err != nil {
		return nil, err
	}

	return appConfigBools(respConfigBools), nil
}

func (r *repo) MarkConfigBoolSended(ctx context.Context, name string) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkConfigBoolSended, argID[string]{
			ID: name,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}

func (r *repo) UpsertAdvertisingCampaignFromManagement(ctx context.Context, advert app.ManagementAdvertisingCampaign) (app.AdvertisingCampaign, error) {
	var resAdvert resAdvertisingCampaign
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &resAdvert, sqlUpsertAdvertisingCampaignFromManagement, argManagementAdvertisingCampaign{
			ID:               advert.ID,
			DefaultDiscount:  advert.DefaultDiscount,
			DiscountPrograms: dalDiscountPrograms(advert.DiscountPrograms),
			EndDate:          advert.EndDate,
			EndMinute:        advert.EndMinute,
			StartDate:        advert.StartDate,
			StartMinute:      advert.StartMinute,
			Weekday:          strings.Join(advert.Weekday, ","),
			Enabled:          advert.Enabled,
			Name:             advert.Name,
			Version:          advert.Version,
			Force:            advert.Force,
		})
		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrSameOrLowerVersion
		}

		return err
	})
	if err != nil {
		return app.AdvertisingCampaign{}, err
	}

	return *appAdvertisingCampaign(resAdvert), nil
}

func (r *repo) NotSendedAdvertisingCampaigns(ctx context.Context) ([]app.AdvertisingCampaign, error) {
	var resCampaigns []resAdvertisingCampaign
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		return tx.SelectContext(ctx, &resCampaigns, sqlNotSendedAdvertisingCampaigns)

	})
	if err != nil {
		return nil, err
	}

	return appAdvertisingCampaigns(resCampaigns), nil
}

func (r *repo) MarkAdvertisingCampaignSended(ctx context.Context, id int64) error {
	return r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExecContext(ctx, sqlMarkAdvertisingCampaignSended, argID[int64]{
			ID: id,
		})

		if errors.Is(err, sql.ErrNoRows) {
			err = app.ErrNotFound
		}

		return err
	})
}
