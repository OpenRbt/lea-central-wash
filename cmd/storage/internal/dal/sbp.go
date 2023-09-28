package dal

import (
	"database/sql"
	"fmt"
	"strconv"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/gofrs/uuid"
	"github.com/powerman/sqlxx"
)

// postID ...
type postID string

// orderID ...
type orderID uuid.UUID

// PaymentsRep ...
type PaymentsRep struct {
}

// dalSbpPayment ...
type dalSbpPayment struct {
	ID               int64          `db:"id"`
	ServerID         uuid.UUID      `db:"server_id"`
	PostID           int32          `db:"post_id"`
	OrderID          uuid.UUID      `db:"order_id"`
	URLPay           sql.NullString `db:"url_pay"`
	Amount           int64          `db:"amount"`
	Canceled         sql.NullBool   `db:"canceled"`
	Confirmed        sql.NullBool   `db:"confirmed"`
	OpenWashReceived sql.NullBool   `db:"openwash_received"`
	CreatedAt        time.Time      `db:"created_at"`
	UpdatedAt        sql.NullTime   `db:"updated_at"`
}

// convertPaymentToDal ...
func convertPaymentToDal(payment app.Payment) dalSbpPayment {
	postID, _ := strconv.ParseInt(payment.PostID, 10, 64)
	return dalSbpPayment{
		ServerID: payment.ServerID,
		PostID:   int32(postID),
		OrderID:  payment.OrderID,
		URLPay: sql.NullString{
			String: payment.UrlPay,
			Valid:  true,
		},
		Amount: payment.Amount,
		Canceled: sql.NullBool{
			Bool:  payment.Canceled,
			Valid: true,
		},
		Confirmed: sql.NullBool{
			Bool:  payment.Confirmed,
			Valid: true,
		},
		OpenWashReceived: sql.NullBool{
			Bool:  payment.OpenwashReceived,
			Valid: true,
		},
		CreatedAt: payment.CreatedAt,
		UpdatedAt: sql.NullTime{
			Time:  payment.UpdatedAt,
			Valid: true,
		},
	}
}

// convertPaymentToApp ...
func convertPaymentToApp(payment dalSbpPayment) app.Payment {
	return app.Payment{
		ServerID:         payment.ServerID,
		PostID:           fmt.Sprintf("%d", payment.PostID),
		OrderID:          payment.OrderID,
		UrlPay:           payment.URLPay.String,
		Amount:           payment.Amount,
		Canceled:         payment.Canceled.Bool,
		Confirmed:        payment.Confirmed.Bool,
		OpenwashReceived: payment.OpenWashReceived.Bool,
		CreatedAt:        payment.CreatedAt,
		UpdatedAt:        payment.UpdatedAt.Time,
	}
}

// interface validation
var _ = app.SbpRepInterface(&repo{})

// SavePayment ...
func (r *repo) SavePayment(req app.Payment) error {

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		arg := convertPaymentToDal(req)
		_, err := tx.NamedExec(
			SavePayment,
			&arg,
		)
		return err
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentURL ...
func (r *repo) SetPaymentURL(orderID uuid.UUID, urlPay string) error {
	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(SetPaymentURL, map[string]interface{}{
			"url_pay":    urlPay,
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		return err
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentCanceled ...
func (r *repo) SetPaymentCanceled(orderID uuid.UUID) (err error) {
	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(SetPaymentCanceled, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentConfirmed ...
func (r *repo) SetPaymentConfirmed(orderID uuid.UUID) (err error) {
	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(SetPaymentConfirmed, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentReceived ...
func (r *repo) SetPaymentReceived(orderID uuid.UUID) (err error) {
	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.NamedExec(SetPaymentReceived, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// GetLastPayment ...
func (r *repo) GetLastPayment(postId string) (app.Payment, error) {
	resp := app.Payment{}
	dbReq := func(tx *sqlxx.Tx) error {
		var res dalSbpPayment
		err := tx.NamedGetContext(ctx, &res, GetLastPayment, map[string]interface{}{
			"post_id": postId,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		resp = convertPaymentToApp(res)
		return nil
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return resp, err
	}

	return resp, nil
}

// GetActualPayments() ...
func (r *repo) GetActualPayments() ([]app.Payment, error) {
	resp := []app.Payment{}
	dbReq := func(tx *sqlxx.Tx) error {
		var res []dalSbpPayment
		err := tx.SelectContext(ctx, &res, GetActualPayments)
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}

		for _, p := range res {
			resp = append(resp, convertPaymentToApp(p))
		}
		return nil
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return resp, err
	}
	return resp, nil
}

// GetPaymentByOrderID ...
func (r *repo) GetPaymentByOrderID(orderID uuid.UUID) (app.Payment, error) {
	resp := app.Payment{}
	dbReq := func(tx *sqlxx.Tx) error {
		var res dalSbpPayment
		err := tx.NamedGetContext(ctx, &res, GetPaymentByOrderID, map[string]interface{}{
			"order_id": orderID,
		})
		switch {
		case err == sql.ErrNoRows:
			return app.ErrNotFound
		case err != nil:
			return err
		}
		resp = convertPaymentToApp(res)
		return nil
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return resp, err
	}

	return resp, nil
}
