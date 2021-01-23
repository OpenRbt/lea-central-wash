package extapi

import (
	"fmt"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

func (svc *service) load(params op.LoadParams) op.LoadResponder {
	log.Info("load", "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	value, err := svc.app.Load(string(params.Args.Hash), *params.Args.Key)
	switch errors.Cause(err) {
	case nil:
		return op.NewLoadOK().WithPayload(string(value))
	case app.ErrNotFound:
		log.Info("load: not found", "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadInternalServerError()
	}
}

func (svc *service) save(params op.SaveParams) op.SaveResponder {
	log.Info("save", "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	err := svc.app.Save(string(params.Args.Hash), *params.Args.KeyPair.Key, *params.Args.KeyPair.Value)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveNoContent()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveInternalServerError()
	}
}

func (svc *service) loadRelay(params op.LoadRelayParams) op.LoadRelayResponder {
	log.Info("load relay", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	toLoad, err := svc.app.LoadRelayReport(string(params.Args.Hash))

	switch errors.Cause(err) {
	case nil:
		return op.NewLoadRelayOK().WithPayload(apiRelayReport(toLoad))
	case app.ErrNotFound:
		log.Info("load relay: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadRelayNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadRelayInternalServerError()
	}
}

func (svc *service) saveRelay(params op.SaveRelayParams) op.SaveRelayResponder {
	log.Info("save relay", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	var toSave app.RelayReport
	toSave.Hash = string(params.Args.Hash)

	for i, _ := range params.Args.RelayStats {
		r := app.RelayStat{
			RelayID:       int(params.Args.RelayStats[i].RelayID),
			SwitchedCount: int(params.Args.RelayStats[i].SwitchedCount),
			TotalTimeOn:   params.Args.RelayStats[i].TotalTimeOn,
		}
		toSave.RelayStats = append(toSave.RelayStats, r)
	}

	err := svc.app.SaveRelayReport(toSave)

	switch errors.Cause(err) {
	case nil:
		return op.NewSaveRelayNoContent()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveRelayInternalServerError()
	}
}

func (svc *service) loadMoney(params op.LoadMoneyParams) op.LoadMoneyResponder {
	log.Info("load money", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	err := app.ErrNotFound
	if params.Args.Hash == "give me report" {
		err = nil
	}

	toLoad, err := svc.app.LoadMoneyReport(string(params.Args.Hash))

	switch errors.Cause(err) {
	case nil:
		return op.NewLoadMoneyOK().WithPayload(apiMoneyReport(toLoad))
	case app.ErrNotFound:
		log.Info("load money: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadMoneyNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadMoneyInternalServerError()
	}
}

func (svc *service) saveMoney(params op.SaveMoneyParams) op.SaveMoneyResponder {
	log.Info("save money", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	var toSave = app.MoneyReport{
		Hash:         string(params.Args.Hash),
		Banknotes:    int(params.Args.Banknotes),
		CarsTotal:    int(params.Args.CarsTotal),
		Coins:        int(params.Args.Coins),
		Electronical: int(params.Args.Electronical),
		Service:      int(params.Args.Service),
		Ctime:        time.Now().UTC(),
	}

	err := svc.app.SaveMoneyReport(toSave)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveMoneyNoContent()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveMoneyInternalServerError()
	}
}

func (svc *service) saveCollection(params op.SaveCollectionParams) op.SaveCollectionResponder {
	log.Info("save collection", "ip", params.HTTPRequest.RemoteAddr)

	var toSave = app.CollectionReport{
		StationID: int(params.Args.ID),
		Money:     int(params.Args.Money),
		Ctime:     time.Now().UTC(),
	}

	fmt.Print("Save collection handler. ID = ")
	fmt.Print(int(params.Args.ID))
	fmt.Print("; Money = ")
	fmt.Println(int(params.Args.Money))

	err := svc.app.SaveCollectionReport(toSave)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveCollectionNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveCollectionInternalServerError()
	}
}

func (svc *service) ping(params op.PingParams) op.PingResponder {
	log.Info("post ping", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	serviceMoney := svc.app.Ping(string(params.Args.Hash))

	return op.NewPingOK().WithPayload(&op.PingOKBody{
		ServiceAmount: newInt64(int64(serviceMoney)),
	})
}

func (svc *service) getPing(params op.GetPingParams) op.GetPingResponder {
	log.Info("get ping", "ip", params.HTTPRequest.RemoteAddr)
	return op.NewGetPingOK()
}

func (svc *service) info(params op.InfoParams) op.InfoResponder {
	return op.NewInfoOK().WithPayload(svc.app.Info())
}

func (svc *service) status(params op.StatusParams) op.StatusResponder {
	report := svc.app.StatusReport()
	return op.NewStatusOK().WithPayload(apiStatusReport(report))
}

func (svc *service) statusCollection(params op.StatusCollectionParams) op.StatusCollectionResponder {
	collection := svc.app.StatusCollection()
	return op.NewStatusCollectionOK().WithPayload(apiStatusCollectionReport(collection))
}

func (svc *service) addServiceAmount(params op.AddServiceAmountParams) op.AddServiceAmountResponder {
	err := svc.app.AddServiceAmount(params.Args.Hash, int(params.Args.Amount))
	switch errors.Cause(err) {
	case nil:
		return op.NewAddServiceAmountNoContent()
	case app.ErrNotFound:
		log.Info("add service ammount: not found", "hash", params.Args.Hash, "amount", params.Args.Amount, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAddServiceAmountNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "amount", params.Args.Amount, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAddServiceAmountInternalServerError()
	}
}

func (svc *service) setStation(params op.SetStationParams) op.SetStationResponder {
	if params.Args.ID == 0 && params.Args.Name == "" {
		return op.NewSetStationUnprocessableEntity()
	}
	err := svc.app.SetStation(app.SetStationParams{
		ID:   int(params.Args.ID),
		Hash: params.Args.Hash,
		Name: params.Args.Name,
	})
	switch errors.Cause(err) {
	case nil:
		return op.NewSetStationNoContent()
	case app.ErrNotFound:
		log.Info("set station: not found", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationNotFound()
	case app.ErrAccessDenied:
		log.Info("set station: access denied", "hash", params.Args.Hash, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationUnauthorized()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationInternalServerError()
	}
}

func (svc *service) delStation(params op.DelStationParams) op.DelStationResponder {
	err := svc.app.DelStation(int(*params.Args.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewDelStationNoContent()
	case app.ErrNotFound:
		log.Info("del station: not found", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewDelStationNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewDelStationInternalServerError()
	}
}

func (svc *service) stationReport(params op.StationReportParams) op.StationReportResponder {
	log.Info("station report", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)

	money, relay, err := svc.app.StationReport(int(*params.Args.ID), time.Unix(*params.Args.StartDate, 0), time.Unix(*params.Args.EndDate, 0))

	switch errors.Cause(err) {
	case nil:
		res := &model.StationReport{}
		res.MoneyReport = apiMoneyReport(&money)
		apiRelay := apiRelayReport(&relay)
		res.RelayStats = apiRelay.RelayStats
		return op.NewStationReportOK().WithPayload(res)
	case app.ErrNotFound:
		log.Info("station report: not found", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportNotFound()
	default:
		log.PrintErr(err, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportInternalServerError()
	}
}

func newInt64(v int64) *int64 {
	return &v
}
