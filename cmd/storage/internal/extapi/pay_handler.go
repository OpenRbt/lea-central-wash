package extapi

import (
	"fmt"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

// pay ...
func (svc *service) pay(params op.PayParams) op.PayResponder {
	if !svc.app.IsSbpRabbitWorkerInit() {
		log.PrintErr("payment request failed: sbp rabbit worker isn't init")
		return op.NewPayInternalServerError()
	}
	var hash string
	if params.Args.Hash != nil {
		hash = *params.Args.Hash
	}

	payAmount := int(params.Args.Amount)

	stationID, err := svc.getID(hash)
	if err != nil {
		return op.NewPayNotFound()
	}

	// logic method
	postID := fmt.Sprintf("%d", stationID)
	err = svc.app.SendPaymentRequest(postID, int64(payAmount))
	if err != nil {
		log.PrintErr("payment request failed:", err, "stationID", stationID, "pay amount", payAmount)

		switch errors.Cause(err) {
		case app.ErrUserIsNotAuthorized:
			return op.NewPayUnauthorized()
		default:
			return op.NewPayInternalServerError()
		}
	}

	return op.NewPayNoContent()
}

// payReceived ...
func (svc *service) payReceived(params op.PayReceivedParams) op.PayReceivedResponder {
	if !svc.app.IsSbpRabbitWorkerInit() {
		log.PrintErr("set payment received failed: sbp rabbit worker isn't init")
		return op.NewPayReceivedInternalServerError()
	}
	var hash string
	if params.Args.Hash != nil {
		hash = *params.Args.Hash
	}

	var qrOrderID string
	if params.Args.QrOrderID != "" {
		qrOrderID = params.Args.QrOrderID
	}

	stationID, err := svc.getID(hash)
	if err != nil {
		return op.NewPayReceivedNotFound()
	}

	// logic method
	err = svc.app.SetPaymentReceived(qrOrderID)
	if err != nil {
		log.PrintErr("set payment received failed:", err, "stationID", stationID, "orderId", qrOrderID)

		switch errors.Cause(err) {
		case app.ErrUserIsNotAuthorized:
			return op.NewPayReceivedUnauthorized()
		default:
			return op.NewPayReceivedInternalServerError()
		}
	}

	return op.NewPayReceivedNoContent()
}
