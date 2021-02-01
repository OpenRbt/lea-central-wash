package extapi

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

func (svc *service) getID(hash string) (app.StationID, error) {
	if hash == app.TestHash {
		return app.TestStationID, nil // For testing purposes
	}
	svc.stationsMutex.Lock()
	defer svc.stationsMutex.Unlock()
	id, ok := svc.stations[hash]
	if !ok {
		return -1, errNotFound
	}
	return id, nil
}

func (svc *service) getHash(id app.StationID) string {
	svc.stationsMutex.Lock()
	defer svc.stationsMutex.Unlock()
	for key, value := range svc.stations {
		if value == id {
			return key
		}
	}
	return ""
}

func (svc *service) getIDAndAddHash(hash string) (app.StationID, error) {
	svc.stationsMutex.Lock()
	defer svc.stationsMutex.Unlock()
	id, ok := svc.stations[hash]
	if !ok {
		svc.unknownHash[hash] = time.Now()
		return -1, errNotFound
	}
	return id, nil
}

func (svc *service) loadHash() error {
	ids, hash, err := svc.repo.LoadHash()
	if err != nil {
		log.Info("loadHash", "err", err)
		return err
	}
	stations := map[string]app.StationID{}
	for i := range ids {
		if hash[i] != "" {
			stations[hash[i]] = ids[i]
		}
	}
	svc.stationsMutex.Lock()
	defer svc.stationsMutex.Unlock()
	for key := range stations {
		if _, ok := svc.unknownHash[key]; ok {
			delete(svc.unknownHash, key)
		}
	}
	svc.stations = stations
	return nil
}

func (svc *service) setHash(id app.StationID, hash string) error {
	stationID, err := svc.getID(hash)
	if err == nil && stationID == id {
		return nil
	}
	err = svc.repo.SetHash(id, hash)
	if err != nil {
		return err
	}
	svc.loadHash()
	return nil
}

func (svc *service) load(params op.LoadParams) op.LoadResponder {
	log.Info("load", "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("load: not found", "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadNotFound()
	}
	value, err := svc.app.Load(stationID, *params.Args.Key)
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
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("save: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveNotFound()
	}

	err = svc.app.Save(stationID, *params.Args.KeyPair.Key, *params.Args.KeyPair.Value)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveNoContent()
	case app.ErrNotFound:
		log.Info("save: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveInternalServerError()
	}
}

func (svc *service) saveIfNotExists(params op.SaveIfNotExistsParams) op.SaveIfNotExistsResponder {
	log.Info("saveIfNotExists", "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("save if not exists: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveIfNotExistsNotFound()
	}

	err = svc.app.SaveIfNotExists(stationID, *params.Args.KeyPair.Key, *params.Args.KeyPair.Value)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveIfNotExistsNoContent()
	case app.ErrNotFound:
		log.Info("save: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveIfNotExistsNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveIfNotExistsInternalServerError()
	}
}

func (svc *service) loadRelay(params op.LoadRelayParams) op.LoadRelayResponder {
	log.Info("load relay", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("load relay: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadRelayNotFound()
	}

	toLoad, err := svc.app.LoadRelayReport(stationID)

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
	var err error
	toSave.StationID, err = svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("save relay: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveRelayNotFound()
	}

	for i := range params.Args.RelayStats {
		r := app.RelayStat{
			RelayID:       int(params.Args.RelayStats[i].RelayID),
			SwitchedCount: int(params.Args.RelayStats[i].SwitchedCount),
			TotalTimeOn:   params.Args.RelayStats[i].TotalTimeOn,
		}
		toSave.RelayStats = append(toSave.RelayStats, r)
	}

	err = svc.app.SaveRelayReport(toSave)

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
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("load money: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadMoneyNotFound()
	}

	toLoad, err := svc.app.LoadMoneyReport(stationID)

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
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("save money: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveMoneyNotFound()
	}

	var toSave = app.MoneyReport{
		StationID:    stationID,
		Banknotes:    int(params.Args.Banknotes),
		CarsTotal:    int(params.Args.CarsTotal),
		Coins:        int(params.Args.Coins),
		Electronical: int(params.Args.Electronical),
		Service:      int(params.Args.Service),
	}

	err = svc.app.SaveMoneyReport(toSave)
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveMoneyNoContent()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveMoneyInternalServerError()
	}
}

func (svc *service) saveCollection(params op.SaveCollectionParams) op.SaveCollectionResponder {
	log.Info("save collection", "stationID", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)

	err := svc.app.SaveCollectionReport(app.StationID(*params.Args.ID))
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
	stationID, err := svc.getIDAndAddHash(string(params.Args.Hash))
	if err != nil {
		log.Info("post ping: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewPingOK().WithPayload(&op.PingOKBody{
			ServiceAmount: newInt64(int64(0)),
		})
	}

	station := svc.app.Ping(stationID)

	return op.NewPingOK().WithPayload(&op.PingOKBody{
		ServiceAmount: newInt64(int64(station.ServiceMoney)),
		OpenStation:   &station.OpenStation,
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
	log.Info("status", "ip", params.HTTPRequest.RemoteAddr)
	report := svc.app.StatusReport()
	return op.NewStatusOK().WithPayload(svc.apiStatusReport(report))
}

func (svc *service) statusCollection(params op.StatusCollectionParams) op.StatusCollectionResponder {
	collection := svc.app.StatusCollection()
	return op.NewStatusCollectionOK().WithPayload(apiStatusCollectionReport(collection))
}

func (svc *service) addServiceAmount(params op.AddServiceAmountParams, auth *storageapi.Profile) op.AddServiceAmountResponder {
	log.Debug("add service ammount", "user", auth.Name)
	log.Info("add service ammount: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("add service ammount: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAddServiceAmountNotFound()
	}

	err = svc.app.AddServiceAmount(*auth, stationID, int(params.Args.Amount))
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
	if *params.Args.ID == 0 && params.Args.Name == "" {
		return op.NewSetStationUnprocessableEntity()
	}
	svc.setHash(app.StationID(*params.Args.ID), params.Args.Hash)
	err := svc.app.SetStation(app.SetStation{
		ID:   app.StationID(*params.Args.ID),
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
	err := svc.app.DelStation(app.StationID(*params.Args.ID))

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

func (svc *service) stationReportDates(params op.StationReportDatesParams) op.StationReportDatesResponder {
	log.Info("station report dates", "id", *params.Args.ID, "start", time.Unix(*params.Args.StartDate, 0), "end", time.Unix(*params.Args.EndDate, 0), "ip", params.HTTPRequest.RemoteAddr)

	money, relay, err := svc.app.StationReportDates(app.StationID(*params.Args.ID), time.Unix(*params.Args.StartDate, 0), time.Unix(*params.Args.EndDate, 0))

	switch errors.Cause(err) {
	case nil:
		res := &model.StationReport{}
		res.MoneyReport = apiMoneyReport(&money)
		apiRelay := apiRelayReport(&relay)
		res.RelayStats = apiRelay.RelayStats
		return op.NewStationReportDatesOK().WithPayload(res)
	case app.ErrNotFound:
		log.Info("station report: not found", "id", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportDatesNotFound()
	default:
		log.PrintErr(err, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportDatesInternalServerError()
	}
}

func (svc *service) stationReportCurrentMoney(params op.StationReportCurrentMoneyParams) op.StationReportCurrentMoneyResponder {
	log.Info("station report current money", "id", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)

	money, relay, err := svc.app.StationReportCurrentMoney(app.StationID(*params.Args.ID))

	switch errors.Cause(err) {
	case nil:
		res := &model.StationReport{}
		res.MoneyReport = apiMoneyReport(&money)
		apiRelay := apiRelayReport(&relay)
		res.RelayStats = apiRelay.RelayStats
		return op.NewStationReportCurrentMoneyOK().WithPayload(res)
	case app.ErrNotFound:
		log.Info("station report: not found", "id", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportCurrentMoneyNotFound()
	default:
		log.PrintErr(err, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportCurrentMoneyInternalServerError()
	}
}

func newInt64(v int64) *int64 {
	return &v
}

func (svc *service) stationByHash(params op.StationByHashParams) op.StationByHashResponder {
	log.Info("post by hash", "hash", params.Args.Hash)

	id, err := svc.getID(string(params.Args.Hash))

	switch errors.Cause(err) {
	case nil:
		return op.NewStationByHashOK().WithPayload(int64(id))
	case errNotFound:
		log.Info("post by hash: not found", "hash", params.Args.Hash)
		return op.NewStationByHashOK().WithPayload(0)
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationByHashInternalServerError()
	}
}

func (svc *service) StationsVariables(params op.StationsVariablesParams) op.StationsVariablesResponder {
	log.Info("stations key pair", "ip", params.HTTPRequest.RemoteAddr)

	toLoad, err := svc.app.StationsVariables()

	switch errors.Cause(err) {
	case nil:
		return op.NewStationsVariablesOK().WithPayload(apiStationsVariables(toLoad))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationsVariablesInternalServerError()
	}
}

func (svc *service) openStation(params op.OpenStationParams) op.OpenStationResponder {
	err := svc.app.OpenStation(app.StationID(*params.Args.StationID))
	switch errors.Cause(err) {
	case nil:
		return op.NewOpenStationNoContent()
	case app.ErrNotFound:
		log.Info("open station: not found", "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewOpenStationNotFound()
	default:
		log.PrintErr(err, "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewOpenStationInternalServerError()
	}
}

func (svc *service) setProgramName(params op.SetProgramNameParams) op.SetProgramNameResponder {
	log.Info("setProgramName", "stationID", *params.Args.StationID, "programID", *params.Args.ProgramID, "name", *params.Args.Name, "ip", params.HTTPRequest.RemoteAddr)
	var err error
	err = svc.app.SetProgramName(app.StationID(*params.Args.StationID), int(*params.Args.ProgramID), *params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewSetProgramNameNoContent()
	default:
		log.PrintErr(err, "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetProgramNameInternalServerError()
	}
}

func (svc *service) setProgramRelays(params op.SetProgramRelaysParams) op.SetProgramRelaysResponder {
	log.Info("setProgramRelays", "stationID", *params.Args.StationID, "programID", *params.Args.ProgramID, "relays", params.Args.Relays, "ip", params.HTTPRequest.RemoteAddr)

	var err error

	tmp := []app.Relay{}

	for i := range params.Args.Relays {
		tmp = append(tmp, app.Relay{
			ID:        int(params.Args.Relays[i].ID),
			TimeOn:    int(params.Args.Relays[i].Timeon),
			TimeOff:   int(params.Args.Relays[i].Timeoff),
			Preflight: int(params.Args.Relays[i].Prfelight),
		})
	}

	err = svc.app.SetProgramRelays(app.StationID(*params.Args.StationID), int(*params.Args.ProgramID), tmp)
	switch errors.Cause(err) {
	case nil:
		return op.NewSetProgramRelaysNoContent()
	default:
		log.PrintErr(err, "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetProgramRelaysInternalServerError()
	}
}

func (svc *service) programs(params op.ProgramsParams) op.ProgramsResponder {
	res, err := svc.app.Programs(app.StationID(*params.Args.StationID))

	switch errors.Cause(err) {
	case nil:
		return op.NewProgramsOK().WithPayload(apiPrograms(res))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewProgramsInternalServerError()
	}
}

func (svc *service) programRelays(params op.ProgramRelaysParams) op.ProgramRelaysResponder {
	var err error
	log.Info("programRelays", "stationID", *params.Args.StationID, "programID", *params.Args.ProgramID, "ip", params.HTTPRequest.RemoteAddr)

	res, err := svc.app.ProgramRelays(app.StationID(*params.Args.StationID), int(*params.Args.ProgramID))

	switch errors.Cause(err) {
	case nil:
		return op.NewProgramRelaysOK().WithPayload(&op.ProgramRelaysOKBody{
			Relays: apiRelays(res),
		})
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewProgramRelaysInternalServerError()
	}
}

func (svc *service) kasse(params op.KasseParams) op.KasseResponder {
	res, err := svc.app.Kasse()

	switch errors.Cause(err) {
	case nil:
		return op.NewKasseOK().WithPayload(apiKasse(res))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewKasseInternalServerError()
	}

}

func (svc *service) setKasse(params op.SetKasseParams) op.SetKasseResponder {
	err := svc.app.SetKasse(app.Kasse{
		CashierFullName: params.Args.Cashier,
		CashierINN:      params.Args.CashierINN,
		TaxType:         params.Args.Tax,
		ReceiptItem:     params.Args.ReceiptItemName,
	})

	log.Info(params.Args)

	switch errors.Cause(err) {
	case nil:
		return op.NewSetKasseNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetKasseInternalServerError()
	}

}
