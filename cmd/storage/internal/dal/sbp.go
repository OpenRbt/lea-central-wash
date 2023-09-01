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
type orderID string

// PaymentsRep ...
type PaymentsRep struct {
	payments        map[postID]*app.Payment
	paymentsByOrder map[orderID]*app.Payment
}

// dalSbpPayment ...
type dalSbpPayment struct {
	ID               int64          `db:"id"`
	ServerID         uuid.UUID      `db:"server_id"`
	PostID           int32          `db:"post_id"`
	OrderID          sql.NullString `db:"order_id"`
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
		ServerID: uuid.FromStringOrNil(payment.ServerID),
		PostID:   int32(postID),
		OrderID: sql.NullString{
			String: payment.OrderId,
			Valid:  true,
		},
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
		ServerID:         payment.ServerID.String(),
		PostID:           fmt.Sprintf("%d", payment.PostID),
		OrderId:          payment.OrderID.String,
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
	r.payments[postID(req.PostID)] = &req
	r.paymentsByOrder[orderID(req.OrderId)] = &req

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		arg := convertPaymentToDal(req)
		_, err := tx.Exec(SavePayment,
			arg.ServerID,
			arg.PostID,
			arg.OrderID,
			arg.URLPay,
			arg.Amount,
			arg.Canceled,
			arg.Confirmed,
			arg.OpenWashReceived,
			arg.CreatedAt,
			arg.UpdatedAt,
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
func (r *repo) SetPaymentURL(orderId string, urlPay string) error {
	val, ok := r.paymentsByOrder[orderID(orderId)]
	if ok {
		val.UpdatedAt = time.Now()
		val.UrlPay = urlPay
	}

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(SetPaymentURL, urlPay, orderId)
		return err
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentCanceled ...
func (r *repo) SetPaymentCanceled(orderId string) (err error) {
	val, ok := r.paymentsByOrder[orderID(orderId)]
	if ok {
		val.UpdatedAt = time.Now()
		val.Canceled = true
	}

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(SetPaymentCanceled, orderId)
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentConfirmed ...
func (r *repo) SetPaymentConfirmed(orderId string) (err error) {
	val, ok := r.paymentsByOrder[orderID(orderId)]
	if ok {
		val.UpdatedAt = time.Now()
		val.Confirmed = true
	}

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(SetPaymentConfirmed, orderId)
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	return nil
}

// SetPaymentReceived ...
func (r *repo) SetPaymentReceived(orderId string) (err error) {
	val, ok := r.paymentsByOrder[orderID(orderId)]
	if ok {
		val.UpdatedAt = time.Now()
		val.OpenwashReceived = true
	}

	// db
	dbReq := func(tx *sqlxx.Tx) error {
		_, err := tx.Exec(SetPaymentReceived, orderId)
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
	val, ok := r.payments[postID(postId)]
	if ok && !val.OpenwashReceived && !val.Canceled {
		resp = *val
	} else {
		dbReq := func(tx *sqlxx.Tx) error {
			var res dalSbpPayment
			err := tx.GetContext(ctx, &res, GetLastPayment, postId)
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
	}
	return resp, nil
}

// GetActualPayments() ...
func (r *repo) GetActualPayments() ([]app.Payment, error) {
	resp := []app.Payment{}
	for _, p := range r.payments {
		if !p.Canceled && p.Confirmed && !p.OpenwashReceived {
			resp = append(resp, *p)
		}
	}
	if len(resp) == 0 {
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
	}
	return resp, nil
}

// GetPaymentByOrderID ...
func (r *repo) GetPaymentByOrderID(orderId string) (app.Payment, error) {
	resp := app.Payment{}
	val, ok := r.paymentsByOrder[orderID(orderId)]
	if ok {
		resp = *val
	} else {
		dbReq := func(tx *sqlxx.Tx) error {
			var res dalSbpPayment
			err := tx.GetContext(ctx, &res, GetPaymentByOrderID, orderId)
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
	}

	return resp, nil
}
