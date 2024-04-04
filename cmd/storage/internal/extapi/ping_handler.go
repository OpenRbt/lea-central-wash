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
	//log.Info("post ping", "time", time.Now(), "hash", *params.Args.Hash, "ip", stationIP)
	stationID, err := svc.getIDAndAddHash(string(*params.Args.Hash))
	if err != nil {
		log.Info("post ping: not found", "hash", params.Args.Hash, "ip", stationIP)
		return op.NewPingOK().WithPayload(&op.PingOKBody{
			ServiceAmount: newInt64(int64(0)),
		})
	}

	station, bonusActive := svc.app.Ping(stationID, int(params.Args.CurrentBalance), int(params.Args.CurrentProgram), stationIP)

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
		BonusSystemActive:   bonusActive,
		AuthorizedSessionID: station.AuthorizedSessionID,
		KaspiAmount:         &station.KaspiMoney,
		// sbp
		QrMoney:         &lastPayment.Amount,
		QrOrderID:       &orderID,
		QrURL:           &lastPayment.UrlPay,
		QrFailed:        &lastPayment.Canceled,
		SbpSystemActive: svc.app.IsSbpAvailableForStation(stationID),
	})
}

func (svc *service) getPing(params op.GetPingParams) op.GetPingResponder {
	log.Info("get ping", "ip", params.HTTPRequest.RemoteAddr)
	return op.NewGetPingOK()
}
