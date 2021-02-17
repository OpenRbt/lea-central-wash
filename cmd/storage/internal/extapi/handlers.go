package extapi

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
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

func (svc *service) saveCollection(params op.SaveCollectionParams, auth *app.Auth) op.SaveCollectionResponder {
	log.Info("save collection", "stationID", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
	log.Debug("save collection", "login", auth.Login, "isAdmin", auth.IsAdmin)

	err := svc.app.SaveCollectionReport(auth, app.StationID(*params.Args.ID))
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

	station := svc.app.Ping(stationID, int(params.Args.CurrentBalance), int(params.Args.CurrentProgram))

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

func (svc *service) statusCollection(params op.StatusCollectionParams, auth *app.Auth) op.StatusCollectionResponder {
	log.Debug("status collection", "login", auth.Login, "isAdmin", auth.IsAdmin)
	collection := svc.app.StatusCollection()
	return op.NewStatusCollectionOK().WithPayload(apiStatusCollectionReport(collection))
}

func (svc *service) addServiceAmount(params op.AddServiceAmountParams) op.AddServiceAmountResponder {
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		log.Info("add service ammount: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAddServiceAmountNotFound()
	}

	err = svc.app.AddServiceAmount(stationID, int(params.Args.Amount))
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

func (svc *service) programs(params op.ProgramsParams) op.ProgramsResponder {
	res, err := svc.app.Programs(params.Args.ProgramID)

	switch errors.Cause(err) {
	case nil:
		return op.NewProgramsOK().WithPayload(apiPrograms(res))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewProgramsInternalServerError()
	}
}

func (svc *service) setProgram(params op.SetProgramParams) op.SetProgramResponder {
	log.Info("setProgram", "programID", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
	var err error
	err = svc.app.SetProgram(appPrograms(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetProgramNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetProgramInternalServerError()
	}
}

func (svc *service) setStationButton(params op.SetStationButtonParams) op.SetStationButtonResponder {
	log.Info("setProgramRelays", "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)

	var err error

	tmp := []app.StationProgram{}

	for i := range params.Args.Buttons {
		tmp = append(tmp, app.StationProgram{
			ProgramID: int(params.Args.Buttons[i].ProgramID),
			ButtonID:  int(params.Args.Buttons[i].ButtonID),
		})
	}

	err = svc.app.SetStationProgram(app.StationID(*params.Args.StationID), tmp)
	switch errors.Cause(err) {
	case nil:
		return op.NewSetStationButtonNoContent()
	case app.ErrUnknownProgram:
		return op.NewSetStationButtonUnprocessableEntity().WithPayload(err.Error())
	case app.ErrUnknownStation:
		return op.NewSetStationButtonUnprocessableEntity().WithPayload(err.Error())
	case app.ErrStationProgramMustUnique:
		return op.NewSetStationButtonUnprocessableEntity().WithPayload(err.Error())
	default:
		log.PrintErr(err, "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationButtonInternalServerError()
	}
}

func (svc *service) stationButton(params op.StationButtonParams) op.StationButtonResponder {
	var err error
	log.Info("StationButton", "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)

	res, err := svc.app.StationProgram(app.StationID(*params.Args.StationID))

	switch errors.Cause(err) {
	case nil:
		return op.NewStationButtonOK().WithPayload(&op.StationButtonOKBody{
			Buttons: apiButtons(res),
		})
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationButtonInternalServerError()
	}
}

func (svc *service) stationProgramByHash(params op.StationProgramByHashParams) op.StationProgramByHashResponder {
	log.Info("StationProgramByHash", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	id, err := svc.getID(string(params.Args.Hash))

	switch errors.Cause(err) {
	case nil:
	case errNotFound:
		log.Info("post by hash: not found", "hash", params.Args.Hash)
		return op.NewStationProgramByHashOK()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationProgramByHashInternalServerError()
	}

	res, err := svc.app.StationConfig(id)

	switch errors.Cause(err) {
	case nil:
		return op.NewStationProgramByHashOK().WithPayload(apiStationConfig((res)))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationProgramByHashInternalServerError()
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

func (svc *service) cardReaderConfig(params op.CardReaderConfigParams) op.CardReaderConfigResponder {
	log.Info("cardReaderConfig", "stationID", *params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
	res, err := svc.app.CardReaderConfig(app.StationID(*params.Args.StationID))

	switch errors.Cause(err) {
	case nil:
		return op.NewCardReaderConfigOK().WithPayload(apiCardReaderConfig(res))
	case app.ErrNotFound:
		return op.NewCardReaderConfigNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewCardReaderConfigInternalServerError()
	}
}

func (svc *service) setCardReaderConfig(params op.SetCardReaderConfigParams) op.SetCardReaderConfigResponder {
	if params.Args.CardReaderType == model.CardReaderConfigCardReaderTypeVENDOTEK && (params.Args.Host == "" || params.Args.Port == "") {
		return op.NewSetCardReaderConfigUnprocessableEntity().WithPayload("host and port required")
	}
	err := svc.app.SetCardReaderConfig(app.CardReaderConfig{
		StationID:      app.StationID(*params.Args.StationID),
		CardReaderType: params.Args.CardReaderType,
		Host:           params.Args.Host,
		Port:           params.Args.Port,
	})

	switch errors.Cause(err) {
	case nil:
		return op.NewSetCardReaderConfigNoContent()
	case app.ErrNotFound:
		return op.NewSetCardReaderConfigNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetCardReaderConfigInternalServerError()
	}

}

func (svc *service) cardReaderConfigByHash(params op.CardReaderConfigByHashParams) op.CardReaderConfigByHashResponder {
	log.Info("cardReaderConfig", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	id, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		return op.NewCardReaderConfigByHashOK().WithPayload(&model.CardReaderConfig{
			StationID:      newInt64(int64(id)),
			CardReaderType: "NOT_USED",
		})
	}

	res, err := svc.app.CardReaderConfig(id)

	switch errors.Cause(err) {
	case nil:
		return op.NewCardReaderConfigByHashOK().WithPayload(apiCardReaderConfig(res))
	case app.ErrNotFound:
		return op.NewCardReaderConfigByHashOK().WithPayload(&model.CardReaderConfig{
			StationID:      newInt64(int64(id)),
			CardReaderType: "NOT_USED",
		})
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewCardReaderConfigByHashInternalServerError()
	}

}

func (svc *service) getUsers(params op.GetUsersParams, auth *app.Auth) op.GetUsersResponder {
	log.Debug("getUsers", "login", auth.Login, "isAdmin", auth.IsAdmin)
	users, err := svc.app.Users(auth)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetUsersOK().WithPayload(apiUsersReport(users))
	case app.ErrAccessDenied:
		return op.NewGetUsersForbidden()
	default:
		return op.NewGetUsersInternalServerError()
	}
}

func (svc *service) getUser(params op.GetUserParams, auth *app.Auth) op.GetUserResponder {
	log.Debug("getUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	return op.NewGetUserOK().WithPayload(apiUserReport(app.UserData{
		Login:      auth.Login,
		FirstName:  &auth.FirstName,
		MiddleName: &auth.MiddleName,
		LastName:   &auth.LastName,
		IsAdmin:    &auth.IsAdmin,
		IsOperator: &auth.IsOperator,
		IsEngineer: &auth.IsEngineer,
	}))
}

func (svc *service) createUser(params op.CreateUserParams, auth *app.Auth) op.CreateUserResponder {
	log.Debug("createUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	id, err := svc.app.CreateUser(app.UserData{
		Login:      string(params.Args.Login),
		Password:   string(params.Args.Password),
		FirstName:  (*string)(params.Args.FirstName),
		MiddleName: (*string)(params.Args.MiddleName),
		LastName:   (*string)(params.Args.LastName),
		IsAdmin:    (*bool)(params.Args.IsAdmin),
		IsEngineer: (*bool)(params.Args.IsEngineer),
		IsOperator: (*bool)(params.Args.IsOperator),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("created user", "id", id)
		return op.NewCreateUserCreated().WithPayload(&op.CreateUserCreatedBody{
			ID: newInt64(int64(id)),
		})
	case app.ErrLoginNotUnique:
		message := err.Error()
		return op.NewCreateUserConflict().WithPayload(&op.CreateUserConflictBody{
			Code:    newInt64(int64(op.CreateUserConflictCode)),
			Message: &message,
		})
	case app.ErrAccessDenied:
		return op.NewCreateUserForbidden()
	default:
		return op.NewCreateUserInternalServerError()
	}

}

func (svc *service) updateUser(params op.UpdateUserParams, auth *app.Auth) op.UpdateUserResponder {
	log.Debug("updateUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	id, err := svc.app.UpdateUser(app.UpdateUserData{
		Login:      string(params.Args.Login),
		FirstName:  (*string)(params.Args.FirstName),
		MiddleName: (*string)(params.Args.MiddleName),
		LastName:   (*string)(params.Args.LastName),
		IsAdmin:    (*bool)(params.Args.IsAdmin),
		IsOperator: (*bool)(params.Args.IsOperator),
		IsEngineer: (*bool)(params.Args.IsEngineer),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("updated user", "id", id)
		return op.NewUpdateUserCreated().WithPayload(&op.UpdateUserCreatedBody{
			ID: newInt64(int64(id)),
		})
	case app.ErrNotFound:
		return op.NewUpdateUserNotFound()
	case app.ErrAccessDenied:
		return op.NewUpdateUserForbidden()
	default:
		return op.NewUpdateUserInternalServerError()
	}
}

func (svc *service) updateUserPassword(params op.UpdateUserPasswordParams, auth *app.Auth) op.UpdateUserPasswordResponder {
	log.Debug("updateUserPassword", "login", auth.Login, "isAdmin", auth.IsAdmin)
	id, err := svc.app.UpdateUserPassword(app.UpdatePasswordData{
		Login:       string(params.Args.Login),
		OldPassword: string(params.Args.OldPassword),
		NewPassword: string(params.Args.NewPassword),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("updated user", "id", id)
		return op.NewUpdateUserPasswordCreated().WithPayload(&op.UpdateUserPasswordCreatedBody{
			ID: newInt64(int64(id)),
		})
	case app.ErrNotFound:
		return op.NewUpdateUserPasswordNotFound()
	case app.ErrAccessDenied:
		return op.NewUpdateUserPasswordForbidden()
	default:
		return op.NewUpdateUserPasswordInternalServerError()
	}
}

func (svc *service) deleteUser(params op.DeleteUserParams, auth *app.Auth) op.DeleteUserResponder {
	log.Debug("deleteUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	err := svc.app.DeleteUser(string(params.Args.Login), auth)
	switch errors.Cause(err) {
	case nil:
		return op.NewDeleteUserNoContent()
	case app.ErrAccessDenied:
		return op.NewDeleteUserForbidden()
	case app.ErrMoneyCollectionFkey:
		message := err.Error()
		return op.NewDeleteUserConflict().WithPayload(&op.DeleteUserConflictBody{
			Code:    newInt64(int64(op.DeleteUserConflictCode)),
			Message: &message,
		})
	default:
		return op.NewDeleteUserInternalServerError()
	}
}
