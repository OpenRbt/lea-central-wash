package extapi

import (
	"net"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/storageapi/restapi/op"
	uuid "github.com/satori/go.uuid"
)

func (svc *service) ping(params op.PingParams) op.PingResponder {
	stationIP, _, err := net.SplitHostPort(params.HTTPRequest.RemoteAddr)
	if err != nil {
		log.Info("post ping: wrong address", "address", params.HTTPRequest.RemoteAddr)
		stationIP = ""
	}
	stationID, err := svc.getIDAndAddHash(string(*params.Args.Hash))
	if err != nil {
		log.Info("post ping: not found", "hash", params.Args.Hash, "ip", stationIP)
		return op.NewPingOK().WithPayload(&op.PingOKBody{
			ServiceAmount: newInt64(int64(0)),
		})
	}

	justTurnedOn := false
	if params.Args.JustTurnedOn != nil {
		justTurnedOn = *params.Args.JustTurnedOn
	}

	station := svc.app.Ping(stationID, int(params.Args.CurrentBalance), int(params.Args.CurrentProgram), stationIP, justTurnedOn)

	var lastPayment app.Payment

	if svc.app.IsSbpRabbitWorkerInit() {
		lastPayment, err = svc.app.GetLastPayment(stationID)
		if err != nil {
			log.Info("get last payment request failed:", "stationID", stationID)
		}

		if lastPayment.OpenwashReceived || lastPayment.Canceled {
			lastPayment = app.Payment{}
		}

		if !lastPayment.Authorized {
			lastPayment.Amount = 0
		}
	}

	orderID := ""
	if lastPayment.OrderID != uuid.Nil {
		orderID = lastPayment.OrderID.String()
	}
	return op.NewPingOK().WithPayload(&op.PingOKBody{
		ServiceAmount:       newInt64(int64(station.ServiceMoney)),
		BonusAmount:         int64(station.BonusMoney),
		OpenStation:         &station.OpenStation,
		ButtonID:            int64(station.ButtonID),
		LastUpdate:          int64(station.LastUpdate),
		LastDiscountUpdate:  station.LastDiscountUpdate,
		SessionID:           station.CurrentSessionID,
		BonusSystemActive:   svc.app.IsBonusAvailable(),
		AuthorizedSessionID: station.AuthorizedSessionID,
		KaspiAmount:         &station.KaspiMoney,
		// sbp
		QrMoney:         &lastPayment.Amount,
		QrOrderID:       &orderID,
		QrURL:           &lastPayment.UrlPay,
		QrFailed:        &lastPayment.Canceled,
		SbpSystemActive: svc.app.IsSbpAvailable(),
	})
}

func (svc *service) getPing(params op.GetPingParams) op.GetPingResponder {
	log.Info("get ping", "ip", params.HTTPRequest.RemoteAddr)
	return op.NewGetPingOK()
}
