package dal

import (
	"database/sql"
	"sync"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/sqlxx"
	uuid "github.com/satori/go.uuid"
)

// orderID ...
type orderID uuid.UUID

// PaymentsRep ...
type PaymentsRep struct {
	LastPayment map[int]*app.Payment
	RWMutex     *sync.RWMutex
}

// SetLastPayment ...
func (paymentsRep *PaymentsRep) SetLastPayment(postID int, p *app.Payment) {
	paymentsRep.RWMutex.Lock()
	paymentsRep.LastPayment[postID] = p
	paymentsRep.RWMutex.Unlock()
}

// GetLastPayment ...
func (paymentsRep *PaymentsRep) GetLastPayment(postID int) (resp app.Payment) {
	paymentsRep.RWMutex.RLock()
	v, ok := paymentsRep.LastPayment[postID]
	paymentsRep.RWMutex.RUnlock()
	if ok && v != nil {
		return *v
	}
	return resp
}

// dalSbpPayment ...
type dalSbpPayment struct {
	ID               int64          `db:"id"`
	Amount           int64          `db:"amount"`
	PostID           int            `db:"post_id"`
	ServerID         uuid.UUID      `db:"server_id"`
	OrderID          uuid.UUID      `db:"order_id"`
	URLPay           sql.NullString `db:"url_pay"`
	Canceled         sql.NullBool   `db:"canceled"`
	Confirmed        sql.NullBool   `db:"confirmed"`
	OpenWashReceived sql.NullBool   `db:"openwash_received"`
	CreatedAt        time.Time      `db:"created_at"`
	UpdatedAt        sql.NullTime   `db:"updated_at"`
}

// convertPaymentToDal ...
func convertPaymentToDal(payment app.Payment) dalSbpPayment {
	return dalSbpPayment{
		ServerID: payment.ServerID,
		PostID:   int(payment.PostID),
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
		PostID:           app.StationID(payment.PostID),
		OrderID:          payment.OrderID,
		UrlPay:           payment.URLPay.String,
		Amount:           payment.Amount,
		Canceled:         payment.Canceled.Bool,
		Confirmed:        payment.Confirmed.Bool,
		OpenwashReceived: payment.OpenWashReceived.Bool,
		CreatedAt:        payment.CreatedAt.UTC(),
		UpdatedAt:        payment.UpdatedAt.Time.UTC(),
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
			savePayment,
			&arg,
		)
		return err
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	r.PaymentsRep.SetLastPayment(int(req.PostID), &req)

	return nil
}

// SetPaymentURL ...
func (r *repo) SetPaymentURL(orderID uuid.UUID, urlPay string) error {
	// db
	postID := int(0)
	dbReq := func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &postID, setPaymentURL, map[string]interface{}{
			"url_pay":    urlPay,
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	}
	err := r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	r.PaymentsRep.SetLastPayment(postID, nil)

	return nil
}

// SetPaymentCanceled ...
func (r *repo) SetPaymentCanceled(orderID uuid.UUID) (err error) {
	// db
	postID := int(0)
	dbReq := func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &postID, setPaymentCanceled, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	r.PaymentsRep.SetLastPayment(postID, nil)

	return nil
}

// SetPaymentConfirmed ...
func (r *repo) SetPaymentConfirmed(orderID uuid.UUID) (err error) {
	// db
	postID := int(0)
	dbReq := func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &postID, setPaymentConfirmed, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}

	r.PaymentsRep.SetLastPayment(postID, nil)

	return nil
}

// SetPaymentReceived ...
func (r *repo) SetPaymentReceived(orderID uuid.UUID) (err error) {
	// db
	postID := int(0)
	dbReq := func(tx *sqlxx.Tx) error {
		err := tx.NamedGetContext(ctx, &postID, setPaymentReceived, map[string]interface{}{
			"order_id":   orderID,
			"updated_at": time.Now().UTC(),
		})
		if err == sql.ErrNoRows {
			return app.ErrNotFound
		}
		return err
	}
	err = r.tx(ctx, nil, dbReq)
	if err != nil {
		return err
	}
	r.PaymentsRep.SetLastPayment(postID, nil)

	return nil
}

// GetLastPayment ...
func (r *repo) GetLastPayment(postID app.StationID) (app.Payment, error) {
	// from ram
	p := r.PaymentsRep.GetLastPayment(int(postID))
	if p.OrderID != uuid.Nil {
		return p, nil
	}

	// from db
	resp := app.Payment{}
	dbReq := func(tx *sqlxx.Tx) error {
		var res dalSbpPayment
		err := tx.NamedGetContext(ctx, &res, getLastPayment, map[string]interface{}{
			"post_id": postID,
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

	r.PaymentsRep.SetLastPayment(int(postID), &resp)

	return resp, nil
}

// GetActualPayments() ...
func (r *repo) GetActualPayments() ([]app.Payment, error) {
	resp := []app.Payment{}
	dbReq := func(tx *sqlxx.Tx) error {
		var res []dalSbpPayment
		err := tx.SelectContext(ctx, &res, getActualPayments)
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
		err := tx.NamedGetContext(ctx, &res, getPaymentByOrderID, map[string]interface{}{
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
