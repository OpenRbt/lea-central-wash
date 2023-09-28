package app

import (
	"errors"
	"fmt"
	"time"

	paymentEntities "github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/sbp-client/entity/payment"
	"github.com/gofrs/uuid"
)

// SbpWorker ...
type SbpWorker struct {
	serverID                     string
	sbpRep                       SbpRepInterface
	sbpBroker                    SbpBrokerInterface
	notificationExpirationPeriod time.Duration
}

// SbpRabbitConfig ...
type SbpRabbitConfig struct {
	ServerID       string
	ServerPassword string
}

// SbpWorkerInterface ...
type SbpWorkerInterface interface {
	// send
	SendPaymentRequest(postID string, amount int64) error
	// set
	SetPaymentURL(orderID uuid.UUID, urlPay string) error
	SetPaymentConfirmed(orderID uuid.UUID) error
	SetPaymentReceived(orderID uuid.UUID) error
	SetPaymentCanceled(orderID uuid.UUID) (err error)
	// get
	GetLastPayment(postID string) (Payment, error)
}

// SbpBrokerInterface ...
type SbpBrokerInterface interface {
	SendPaymentRequest(payRequest paymentEntities.PayRequest) (err error)
	CancelPayment(payСancellationRequest paymentEntities.PayСancellationRequest) (err error)
}

// SbpRepInterface ...
type SbpRepInterface interface {
	SavePayment(req Payment) error

	SetPaymentURL(orderID uuid.UUID, urlPay string) error
	SetPaymentConfirmed(orderID uuid.UUID) (err error)
	SetPaymentReceived(orderID uuid.UUID) (err error)
	SetPaymentCanceled(orderID uuid.UUID) (err error)

	GetLastPayment(postID string) (Payment, error)
	GetPaymentByOrderID(orderID uuid.UUID) (Payment, error)
	GetActualPayments() ([]Payment, error)
}

var _ = SbpWorkerInterface(&SbpWorker{})

// Payment ...
type Payment struct {
	ServerID         uuid.UUID
	OrderID          uuid.UUID
	PostID           string
	UrlPay           string
	Amount           int64
	Canceled         bool
	Confirmed        bool
	OpenwashReceived bool
	CreatedAt        time.Time
	UpdatedAt        time.Time
}

// CancelExpiratedNotOpenwashReceivedPayments ...
func (w *SbpWorker) CancelExpiratedNotOpenwashReceivedPayments() {
	// minute
	t := time.NewTicker(time.Minute)
	for {
		// get last payments
		reqs, err := w.sbpRep.GetActualPayments()
		if err != nil {
			err = fmt.Errorf("process messages failed: %w", err)
			log.Err(err)
		}

		// cancel
		<-t.C
		for i := 0; i < len(reqs); i++ {
			// expiration check
			period := time.Since(reqs[i].UpdatedAt.UTC()).Abs()
			if period >= w.notificationExpirationPeriod && !reqs[i].UpdatedAt.IsZero() {
				err := w.paymentCancel(reqs[i].ServerID, reqs[i].PostID, reqs[i].OrderID)
				if err != nil {
					err = fmt.Errorf("process messages orderID = %s failed: %w", reqs[i].OrderID, err)
					if log.Err(err) != nil {
						fmt.Println(err)
					}
				}
			}
		}
	}
}

// SendPaymentRequest ...
func (w *SbpWorker) SendPaymentRequest(postID string, amount int64) error {
	if amount <= 0 {
		return fmt.Errorf("send payment request failed: amount <= 0")
	}

	orderID, err := uuid.NewV4()
	if err != nil {
		return fmt.Errorf("send pay request failed: %w", err)
	}

	dbReq := Payment{
		ServerID:         uuid.FromStringOrNil(w.serverID),
		PostID:           postID,
		OrderID:          orderID,
		UrlPay:           "",
		Amount:           amount,
		Canceled:         false,
		Confirmed:        false,
		OpenwashReceived: false,
		CreatedAt:        time.Now(),
		UpdatedAt:        time.Now(),
	}
	err = w.sbpRep.SavePayment(dbReq)
	if err != nil {
		return fmt.Errorf("send pay request failed: %w", err)
	}

	brokerReq := paymentEntities.PayRequest{
		Amount:  amount,
		WashID:  w.serverID,
		PostID:  postID,
		OrderID: dbReq.OrderID.String(),
	}
	err = w.sbpBroker.SendPaymentRequest(brokerReq)
	if err != nil {
		return fmt.Errorf("send payment request failed: %w", err)
	}

	return nil
}

// SetPaymentURL ...
func (w *SbpWorker) SetPaymentURL(orderID uuid.UUID, urlPay string) error {
	if orderID.IsNil() {
		return errors.New("SetPaymentURL: orderID = nil")
	}
	if urlPay == "" {
		return errors.New("SetPaymentURL: urlPay is empty")
	}
	err := w.sbpRep.SetPaymentURL(orderID, urlPay)
	if err != nil {
		return fmt.Errorf("set payment url failed: %w", err)
	}

	return nil
}

// SetPaymentCanceled ...
func (w *SbpWorker) SetPaymentCanceled(orderID uuid.UUID) error {
	if orderID.IsNil() {
		return errors.New("SetPaymentCanceled: orderID = nil")
	}
	err := w.sbpRep.SetPaymentCanceled(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	return nil
}

// SetPaymentConfirmed ...
func (w *SbpWorker) SetPaymentConfirmed(orderID uuid.UUID) error {
	if orderID.IsNil() {
		return errors.New("SetPaymentConfirmed: orderID = nil")
	}
	err := w.sbpRep.SetPaymentConfirmed(orderID)
	if err != nil {
		return fmt.Errorf("set payment confirmed failed: %w", err)
	}

	return nil
}

// SetPaymentReceived ...
func (w *SbpWorker) SetPaymentReceived(orderID uuid.UUID) error {
	if orderID.IsNil() {
		return errors.New("SetPaymentReceived: orderID = nil")
	}
	// get payment request by orderID
	payment, err := w.sbpRep.GetPaymentByOrderID(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	// check is it confirmed
	if !payment.Confirmed {
		return errors.New("set payment received failed: payment is not confirmed")
	}

	// set payment received
	err = w.sbpRep.SetPaymentReceived(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	return nil
}

// GetLastPayment ...
func (w *SbpWorker) GetLastPayment(postID string) (Payment, error) {
	payment, err := w.sbpRep.GetLastPayment(postID)
	if err != nil {
		return Payment{}, fmt.Errorf("get last payment failed: %w", err)
	}

	return payment, nil
}

// paymentCancel ...
func (w *SbpWorker) paymentCancel(serverID uuid.UUID, postID string, orderID uuid.UUID) error {
	if orderID.IsNil() {
		return errors.New("paymentCancel: orderID = nil")
	}
	if !orderID.IsNil() {
		cancelReq := paymentEntities.PayСancellationRequest{
			WashID:  serverID.String(),
			PostID:  postID,
			OrderID: orderID.String(),
		}

		// cancelPayment
		err := w.sbpBroker.CancelPayment(cancelReq)
		if err != nil {
			return fmt.Errorf("cancel orderID = %s failed: %w", orderID, err)
		}
	}

	// set payment canceled
	err := w.sbpRep.SetPaymentCanceled(orderID)
	if err != nil {
		return fmt.Errorf("expiration check by orderID = %s failed: %w", orderID, err)
	}

	return nil
}
