package app

import (
	"fmt"
	"time"

	uuid "github.com/satori/go.uuid"
	"github.com/shopspring/decimal"
)

type LeaStatus = int

const (
	LeaStatusOk              LeaStatus = 1
	LeaStatusStationOffline  LeaStatus = 2
	LeaStatusStationNotFound LeaStatus = 3
	LeaStatusWashTimeout     LeaStatus = 4
	LeaStatusOtherErr        LeaStatus = 5
	LeaStatusAccountNotFound LeaStatus = 6
)

type CommandType = int

const (
	CommandCheck CommandType = 1
	CommandPay   CommandType = 2
)

type Command struct {
	MessageID    uuid.UUID
	WashServerID uuid.UUID
	StationID    int64
	Type         CommandType
	Account      string
	TxnID        int64
	TxnDate      time.Time
	Sum          decimal.Decimal
	IP           string
}

type KaspiAnswer struct {
	MessageID uuid.UUID
	Status    LeaStatus
	Info      string
}

func (a *app) InitKaspi(svc KaspiService) {
	a.kaspiSvc = svc
}

func (a *app) IsKaspiInit() bool {
	return a.kaspiSvc != nil
}

func (a *app) IsKaspiAvailable() bool {
	if !a.IsKaspiInit() {
		return false
	}
	status := a.kaspiSvc.Status()
	return status.IsAvailable()
}

func (a *app) KaspiCommand(cmd Command) {
	fmt.Printf("KaspiCommand %v\n", cmd)
	switch cmd.Type {
	case CommandCheck:
		a.kaspiSvc.SendAnswer(a.kaspiCheck(cmd))
		return
	case CommandPay:
		a.kaspiPay(cmd)
		return
	default:
		a.kaspiSvc.SendAnswer(KaspiAnswer{
			MessageID: cmd.MessageID,
			Status:    LeaStatusOtherErr,
			Info:      fmt.Sprintf("unknown command type: %v", cmd.Type),
		})
	}
}

func (a *app) kaspiCheck(cmd Command) KaspiAnswer {
	station, err := a.Get(StationID(cmd.StationID))
	if err == ErrNotFound || !station.IsActive {
		return KaspiAnswer{
			MessageID: cmd.MessageID,
			Status:    LeaStatusStationNotFound,
		}
	}
	if err != nil {
		return KaspiAnswer{
			MessageID: cmd.MessageID,
			Status:    LeaStatusOtherErr,
			Info:      err.Error(),
		}
	}

	if time.Since(station.LastPing).Seconds() > 5 {
		return KaspiAnswer{
			MessageID: cmd.MessageID,
			Status:    LeaStatusStationOffline,
		}
	}
	return KaspiAnswer{
		MessageID: cmd.MessageID,
		Status:    LeaStatusOk,
	}
}

func (a *app) kaspiPay(cmd Command) {
	check := a.kaspiCheck(cmd)
	if check.Status != LeaStatusOk {
		a.kaspiSvc.SendAnswer(check)
		return
	}
	err := a.kaspiSvc.SendAnswer(KaspiAnswer{
		MessageID: cmd.MessageID,
		Status:    LeaStatusOk,
	})
	if err == nil {
		a.AddKaspiAmount(StationID(cmd.StationID), cmd.Sum.IntPart())
	}
}
