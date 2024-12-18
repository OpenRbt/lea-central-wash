package app

import (
	"errors"
	"fmt"
	"time"

	uuid "github.com/satori/go.uuid"
)

// SbpWorker ...
type SbpWorker struct {
	serverID                      string
	sbpRep                        SbpRepInterface
	sbpBroker                     SbpBrokerInterface
	notificationExpirationPeriod  time.Duration
	paymentConfirmationPingPeriod time.Duration

	sendConfirmRequestChan  chan struct{}
	sendConfirmRequestTiker *time.Ticker
}

// SbpRabbitConfig ...
type SbpRabbitConfig struct {
	ServerID       string
	ServerPassword string
}

// SbpWorkerInterface ...
type SbpWorkerInterface interface {
	// send
	SendPaymentRequest(postID StationID, amount int64) error
	// set
	SetPaymentURL(orderID uuid.UUID, urlPay string) error
	ReceiveNotification(orderID uuid.UUID, status PaymentStatus) error
	SetPaymentReceived(orderID uuid.UUID) error
	SetPaymentCanceled(orderID uuid.UUID) (err error)
	// get
	GetLastPayment(postID StationID) (Payment, error)
}

// SbpBrokerInterface ...
type SbpBrokerInterface interface {
	SendPaymentRequest(Payment) error
	CancelPayment(Payment Payment, errMsg string) error
	ConfirmPayment(Payment) error
	Status() ServiceStatus
	Ping(serverID string, status []StationPingStatus) error
	RequestServiceStatus() error
}

// SbpRepInterface ...
type SbpRepInterface interface {
	SavePayment(req Payment) error

	SetPaymentURL(orderID uuid.UUID, urlPay string) error
	SetPaymentAuthorized(orderID uuid.UUID) (err error)
	SetPaymentReceived(orderID uuid.UUID) (err error)
	SetPaymentCanceled(orderID uuid.UUID) (err error)

	UpdatePayment(orderID uuid.UUID, update PaymentUpdate) (err error)
	GetLastPayment(postID StationID) (Payment, error)
	GetPaymentByOrderID(orderID uuid.UUID) (Payment, error)
	GetActualPayments() ([]Payment, error)
	GetPaymentsForConfirmAgain() ([]Payment, error)
	GetPaymentsForConfirm() ([]Payment, error)
}

var _ = SbpWorkerInterface(&SbpWorker{})

// Payment ...
type Payment struct {
	ServerID         uuid.UUID
	OrderID          uuid.UUID
	PostID           StationID
	UrlPay           string
	Amount           int64
	Canceled         bool
	Authorized       bool
	OpenwashReceived bool
	CreatedAt        time.Time
	UpdatedAt        time.Time
	Confirmed        bool
	LastConfirmedAt  *time.Time
	SentConfirmed    bool
}

type PaymentUpdate struct {
	URLPay           *string
	Canceled         *bool
	Confirmed        *bool
	OpenWashReceived *bool
	Authorized       *bool
	LastConfirmedAt  *time.Time
	UpdatedAt        *time.Time
	SentConfirmed    *bool
}

type PaymentStatus string

const (
	PaymentStatusAuthorized PaymentStatus = "authorized"
	PaymentStatusConfirmed  PaymentStatus = "confirmed"
	PaymentStatusCanceled   PaymentStatus = "canceled"
	PaymentStatusRejected   PaymentStatus = "rejected"
	PaymentStatusReversed   PaymentStatus = "reversed"
	PaymentStatusRefunded   PaymentStatus = "refunded"
)

// Repeat ...
func Repeat(f func(), duration time.Duration) {
	// minute
	t := time.NewTicker(duration)
	for {
		<-t.C
		f()
	}
}

func (w *SbpWorker) CancelExpiratedNotOpenwashReceivedPayments() {
	// get last payments
	reqs, err := w.sbpRep.GetActualPayments()
	if err != nil {
		log.Err(fmt.Errorf("process messages failed: %w", err))
	}

	for _, req := range reqs {
		period := time.Since(req.UpdatedAt.UTC()).Abs()
		if period >= w.notificationExpirationPeriod && !req.UpdatedAt.IsZero() {
			err := w.paymentCancel(req.ServerID, req.PostID, req.OrderID)
			if err != nil {
				log.Err(fmt.Errorf("process messages orderID = %s failed: %w", req.OrderID, err))
			}
		}
	}

	reqs, err = w.sbpRep.GetPaymentsForConfirmAgain()
	if err != nil {
		log.Err(fmt.Errorf("process messages failed: %w", err))
	}

	for _, req := range reqs {
		if req.LastConfirmedAt == nil || time.Since((*req.LastConfirmedAt).UTC()) >= w.paymentConfirmationPingPeriod {
			err := w.sbpBroker.ConfirmPayment(Payment{OrderID: req.OrderID})
			if err != nil {
				log.Err(fmt.Errorf("process messages orderID = %s failed: %w", req.OrderID, err))
			}

			lastConfirmedAt := time.Now().UTC()
			err = w.sbpRep.UpdatePayment(req.OrderID, PaymentUpdate{LastConfirmedAt: &lastConfirmedAt})
			if err != nil {
				log.Err(fmt.Errorf("process messages orderID = %s failed: %w", req.OrderID, err))
			}
		}
	}
}

func (w *SbpWorker) confirmPayment() {
	for {
		select {
		case <-w.sendConfirmRequestChan:
		case <-w.sendConfirmRequestTiker.C:
		}

		reqs, err := w.sbpRep.GetPaymentsForConfirm()
		if err != nil {
			log.Err(fmt.Errorf("process messages failed: %w", err))
		}

		for _, payment := range reqs {
			err := w.sbpBroker.ConfirmPayment(Payment{OrderID: payment.OrderID})
			if err != nil {
				log.Err(fmt.Errorf("send payment confirmation in rabbir failed: %w", err))
				continue
			}

			lastConfirmedAt := time.Now().UTC()
			sentConfirm := true
			err = w.sbpRep.UpdatePayment(payment.OrderID, PaymentUpdate{LastConfirmedAt: &lastConfirmedAt, SentConfirmed: &sentConfirm})
			if err != nil {
				log.Err(fmt.Errorf("process messages orderID = %s failed: %w", payment.OrderID, err))
				continue
			}
		}
	}
}

// SendPaymentRequest ...
func (w *SbpWorker) SendPaymentRequest(postID StationID, amount int64) error {
	if amount <= 0 {
		return fmt.Errorf("send payment request failed: amount <= 0")
	}

	status := w.sbpBroker.Status()
	if !status.IsAvailable() {
		return ErrServiceNotAvailable
	}

	orderID := uuid.NewV4()

	t := time.Now()
	dbReq := Payment{
		ServerID:         uuid.FromStringOrNil(w.serverID),
		PostID:           postID,
		OrderID:          orderID,
		UrlPay:           "",
		Amount:           amount,
		Canceled:         false,
		Confirmed:        false,
		OpenwashReceived: false,
		CreatedAt:        t,
		UpdatedAt:        t,
	}
	err := w.sbpRep.SavePayment(dbReq)
	if err != nil {
		return fmt.Errorf("send pay request failed: %w", err)
	}
	go func() {
		i := 0
		for i < 1 {
			err = w.sbpBroker.SendPaymentRequest(dbReq)
			if err == nil {
				break
			}
			if err == ErrSendTimeout {
				log.Err(err)
				break
			}
			i++
			log.Err("send payment request failed:", "error", err, "attemp", i)
			time.Sleep(time.Duration(500*i) * time.Millisecond)
		}
	}()
	return nil
}

// SetPaymentURL ...
func (w *SbpWorker) SetPaymentURL(orderID uuid.UUID, urlPay string) error {
	if orderID == uuid.Nil {
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
	if orderID == uuid.Nil {
		return errors.New("SetPaymentCanceled: orderID = nil")
	}
	err := w.sbpRep.SetPaymentCanceled(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	return nil
}

// SetPaymentConfirmed ...
func (w *SbpWorker) ReceiveNotification(orderID uuid.UUID, status PaymentStatus) error {
	if orderID == uuid.Nil {
		return errors.New("SetPaymentConfirmed: orderID = nil")
	}
	_, err := w.sbpRep.GetPaymentByOrderID(orderID)
	if err != nil {
		return fmt.Errorf("set payment confirmed failed: %w", err)
	}

	switch status {
	case PaymentStatusAuthorized:
		err := w.sbpRep.SetPaymentAuthorized(orderID)
		if err != nil {
			return fmt.Errorf("set payment confirmed failed: %w", err)
		}
	case PaymentStatusConfirmed:
		confirmed := true
		err = w.sbpRep.UpdatePayment(orderID, PaymentUpdate{Confirmed: &confirmed})
		if err != nil {
			return fmt.Errorf("set payment confirmed failed: %w", err)
		}
	case PaymentStatusCanceled, PaymentStatusRejected, PaymentStatusReversed, PaymentStatusRefunded:
		err = w.sbpRep.SetPaymentCanceled(orderID)
		if err != nil {
			return fmt.Errorf("set payment canceled failed: %w", err)
		}
	}

	return nil
}

// SetPaymentReceived ...
func (w *SbpWorker) SetPaymentReceived(orderID uuid.UUID) error {
	if orderID == uuid.Nil {
		return errors.New("SetPaymentReceived: orderID = nil")
	}
	// get payment request by orderID
	payment, err := w.sbpRep.GetPaymentByOrderID(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	// check is it confirmed
	if !payment.Authorized {
		return errors.New("set payment received failed: payment is not confirmed")
	}

	// set payment received
	err = w.sbpRep.SetPaymentReceived(orderID)
	if err != nil {
		return fmt.Errorf("set payment received failed: %w", err)
	}

	w.sendConfirmRequestChan <- struct{}{}

	return nil
}

// GetLastPayment ...
func (w *SbpWorker) GetLastPayment(postID StationID) (Payment, error) {
	payment, err := w.sbpRep.GetLastPayment(postID)
	if err != nil {
		return Payment{}, fmt.Errorf("get last payment failed: %w", err)
	}

	return payment, nil
}

// paymentCancel ...
func (w *SbpWorker) paymentCancel(serverID uuid.UUID, postID StationID, orderID uuid.UUID) error {
	if orderID == uuid.Nil {
		return errors.New("paymentCancel: orderID = nil")
	}

	// cancelPayment
	err := w.sbpBroker.CancelPayment(Payment{
		ServerID: serverID,
		PostID:   postID,
		OrderID:  orderID,
	}, "time out")
	if err != nil {
		return fmt.Errorf("cancel orderID = %s failed: %w", orderID, err)
	}

	// set payment canceled
	err = w.sbpRep.SetPaymentCanceled(orderID)
	if err != nil {
		return fmt.Errorf("expiration check by orderID = %s failed: %w", orderID, err)
	}

	return nil
}

func (w *SbpWorker) Ping(serverID string, status []StationPingStatus) error {
	return w.sbpBroker.Ping(serverID, status)
}
