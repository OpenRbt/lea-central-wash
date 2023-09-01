package extapi

import (
	"fmt"
	"net"

	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
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

	stationIDString := fmt.Sprintf("%d", stationID)
	lastPayment, err := svc.app.GetLastPayment(stationIDString)
	if err != nil {
		log.Info("post ping: failed get last payment request", "stationID", stationIDString)
		stationIP = ""
	}

	var amount int64
	if lastPayment.Confirmed && !lastPayment.Canceled {
		amount = lastPayment.Amount
	}

	return op.NewPingOK().WithPayload(&op.PingOKBody{
		ServiceAmount:       newInt64(int64(station.ServiceMoney)),
		BonusAmount:         int64(station.BonusMoney),
		OpenStation:         &station.OpenStation,
		ButtonID:            int64(station.ButtonID),
		LastUpdate:          int64(station.LastUpdate),
		LastDiscountUpdate:  int64(station.LastDiscountUpdate),
		SessionID:           station.CurrentSessionID,
		BonusSystemActive:   bonusActive,
		AuthorizedSessionID: station.AuthorizedSessionID,
		// sbp
		QrMoney:   &amount,
		QrOrderID: &lastPayment.OrderId,
		QrURL:     &lastPayment.UrlPay,
		QrFailed:  &lastPayment.Canceled,
	})
}

func (svc *service) getPing(params op.GetPingParams) op.GetPingResponder {
	log.Info("get ping", "ip", params.HTTPRequest.RemoteAddr)
	return op.NewGetPingOK()
}
