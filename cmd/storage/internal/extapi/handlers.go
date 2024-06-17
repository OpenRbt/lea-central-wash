package extapi

import (
	"fmt"
	"strings"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	"github.com/OpenRbt/lea-central-wash/storageapi/model"
	"github.com/OpenRbt/lea-central-wash/storageapi/restapi/op"
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
	//log.Info("load", "hash", *params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(*params.Args.Hash))
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

func (svc *service) loadFromStation(params op.LoadFromStationParams) op.LoadFromStationResponder {
	log.Info("loadFromStation", "hash", params.Args.Hash, "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	value, err := svc.app.Load(app.StationID(*params.Args.StationID), *params.Args.Key)
	switch errors.Cause(err) {
	case nil:
		return op.NewLoadFromStationOK().WithPayload(string(value))
	case app.ErrNotFound:
		log.Info("loadFromStation: not found", "hash", params.Args.Hash, "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadFromStationNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadFromStationInternalServerError()
	}
}

func (svc *service) save(params op.SaveParams) op.SaveResponder {
	log.Info("save", "hash", *params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(*params.Args.Hash))
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
	log.Info("saveIfNotExists", "hash", *params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(*params.Args.Hash))
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
	return op.NewLoadRelayOK()
}

func (svc *service) saveRelay(params op.SaveRelayParams) op.SaveRelayResponder {
	return op.NewSaveRelayNoContent()
}

func (svc *service) loadMoney(params op.LoadMoneyParams) op.LoadMoneyResponder {
	log.Info("load money", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(*params.Args.Hash))
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
	log.Info("save money", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	stationID, err := svc.getID(string(*params.Args.Hash))
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
		Bonuses:      int(params.Args.Bonuses),
		SessionID:    params.Args.SessionID,
		QrMoney:      int(params.Args.QrMoney),
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

func (svc *service) info(params op.InfoParams) op.InfoResponder {
	return op.NewInfoOK().WithPayload(svc.app.Info())
}

func (svc *service) status(params op.StatusParams) op.StatusResponder {
	//log.Info("status", "ip", params.HTTPRequest.RemoteAddr)
	report := svc.app.StatusReport(false)
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

func (svc *service) station(params op.StationParams) op.StationResponder {
	res, err := svc.app.Station(app.StationID(*params.Args.ID))
	switch errors.Cause(err) {
	case nil:
		return op.NewStationOK().WithPayload(&model.StationConfig{
			ID:           newInt64(int64(res.ID)),
			Name:         res.Name,
			PreflightSec: int64(res.PreflightSec),
			RelayBoard:   model.RelayBoard(res.RelayBoard),
			Hash:         svc.getHash(res.ID),
		})
	case app.ErrNotFound:
		log.Info("set station: not found", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationNotFound()
	case app.ErrAccessDenied:
		log.Info("set station: access denied", "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationUnauthorized()
	default:
		log.PrintErr(err, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationInternalServerError()
	}
}

func (svc *service) setStation(params op.SetStationParams) op.SetStationResponder {
	if *params.Args.ID == 0 && params.Args.Name == "" {
		return op.NewSetStationUnprocessableEntity()
	}
	svc.setHash(app.StationID(*params.Args.ID), params.Args.Hash)
	err := svc.app.SetStation(app.SetStation{
		ID:           app.StationID(*params.Args.ID),
		Name:         params.Args.Name,
		PreflightSec: int(params.Args.PreflightSec),
		RelayBoard:   string(params.Args.RelayBoard),
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
		log.Info("station report dates: not found", "id", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
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
		log.Info("station report current money: not found", "id", *params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportCurrentMoneyNotFound()
	default:
		log.PrintErr(err, "id", params.Args.ID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationReportCurrentMoneyInternalServerError()
	}
}

func (svc *service) stationCollectionReportDates(params op.StationCollectionReportDatesParams, auth *app.Auth) op.StationCollectionReportDatesResponder {
	log.Info("station  collection reports by dates", "id", params.Args.StationID, "startDate", params.Args.StartDate, "endDate", params.Args.EndDate, "ip", params.HTTPRequest.RemoteAddr)

	var startDate *time.Time
	if params.Args.StartDate != nil {
		CreateDateStart := time.Unix(*params.Args.StartDate, 0)
		startDate = &CreateDateStart
	}
	var endDate *time.Time
	if params.Args.EndDate != nil {
		CreateDateEnd := time.Unix(*params.Args.EndDate, 0)
		endDate = &CreateDateEnd
	}
	reports, err := svc.app.CollectionReports(app.StationID(params.Args.StationID), startDate, endDate)

	switch errors.Cause(err) {
	case nil:
		return op.NewStationCollectionReportDatesOK().WithPayload(&op.StationCollectionReportDatesOKBody{
			CollectionReports: apiCollectionReportWithUser(reports),
		})
	case app.ErrNotFound:
		log.Info("station report: not found", "id", params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationCollectionReportDatesNotFound()
	default:
		log.PrintErr(err, "id", params.Args.StationID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationCollectionReportDatesInternalServerError()
	}
}
func newInt64(v int64) *int64 {
	return &v
}

func (svc *service) stationByHash(params op.StationByHashParams) op.StationByHashResponder {
	log.Info("post by hash", "hash", *params.Args.Hash)

	id, err := svc.getID(string(*params.Args.Hash))

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
	err := svc.app.SetProgram(appPrograms(params.Args))
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
	log.Info("StationProgramByHash", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	id, err := svc.getID(string(*params.Args.Hash))

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

func (svc *service) runProgram(params op.RunProgramParams) op.RunProgramResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		log.Info("runProgram: not found", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRunProgramNotFound().WithPayload("station not found")
	}
	err = svc.app.RunProgram(stationID, *params.Args.ProgramID, *params.Args.Preflight)

	// log.Info("runProgram", "programID", *params.Args.ProgramID, "stationID", stationID, "preflight", *params.Args.Preflight, "ip", params.HTTPRequest.RemoteAddr)

	switch errors.Cause(err) {
	case nil:
		return op.NewRunProgramNoContent()
	case app.ErrNotFound:
		// log.PrintErr(err, "hash", params.Args.Hash, "stationID", stationID, "programID", *params.Args.ProgramID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRunProgramNotFound().WithPayload("program or relay board not found")
	default:
		// log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRunProgramInternalServerError()
	}
}

func (svc *service) run2Program(params op.Run2ProgramParams) op.Run2ProgramResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		log.Info("runProgram: not found", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRun2ProgramNotFound().WithPayload("station not found")
	}
	err = svc.app.Run2Program(stationID, *params.Args.ProgramID, *params.Args.ProgramID2, *params.Args.Preflight)
	log.Info("Error run2programm is - ", err)

	log.Info("runProgram", "programID", *params.Args.ProgramID, "programID2", *params.Args.ProgramID2, "stationID", stationID, "preflight", *params.Args.Preflight, "ip", params.HTTPRequest.RemoteAddr)

	switch errors.Cause(err) {
	case nil:
		return op.NewRun2ProgramNoContent()
	case app.ErrNotFound:
		log.PrintErr(err, "hash", params.Args.Hash, "stationID", stationID, "programID", *params.Args.ProgramID, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRun2ProgramNotFound().WithPayload("program or relay board not found")
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewRun2ProgramInternalServerError()
	}
}

func (svc *service) dispenserStop(params op.DispenserStopParams) op.DispenserStopResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		log.Info("DispenserPause: StationID not found ", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	}

	err = svc.app.DispenserStop(stationID, *params.Args.StopProgramID)

	log.Info("Program Pause ", "ip", params.HTTPRequest.RemoteAddr)

	switch errors.Cause(err) {
	case nil:
		return op.NewDispenserStopNoContent()
	case app.ErrNotFound:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewDispenserStopNotFound().WithPayload("Arduino not found")
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewDispenserStopInternalServerError()
	}
}

func (svc *service) measureVolumeMilliliters(params op.MeasureVolumeMillilitersParams) op.MeasureVolumeMillilitersResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		log.Info("measureVolumeMilliliters: StationID not found ", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	}

	err = svc.app.MeasureVolumeMilliliters(*params.Args.Volume, stationID, *params.Args.StartProgramID, *params.Args.StopProgramID)

	log.Info("Run Command Dispenser ", "Volume", *params.Args.Volume, "ip", params.HTTPRequest.RemoteAddr)
	fmt.Println("ERROR: ", err)

	switch errors.Cause(err) {
	case nil:
		return op.NewMeasureVolumeMillilitersNoContent()
	case app.ErrNotFound:
		log.PrintErr(err, "hash", params.Args.Hash, "Volume", params.Args.Volume, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewMeasureVolumeMillilitersNotFound().WithPayload("Arduino not found")
	case app.ErrNotFoundDispenser:
		log.PrintErr(err, "hash", params.Args.Hash, "Volume", params.Args.Volume, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewMeasureVolumeMillilitersNotFound().WithPayload("Arduino not found")
	case app.ErrNotFoundBoard:
		log.PrintErr(err, "hash", params.Args.Hash, "Volume", params.Args.Volume, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewMeasureVolumeMillilitersNotFound().WithPayload("Board not found")
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewMeasureVolumeMillilitersInternalServerError()
	}
}

func (svc *service) VolumeDispenser(params op.VolumeDispenserParams) op.VolumeDispenserResponder {

	volume, status, err := svc.app.GetVolumeDispenser()

	log.Info("Get Volume", "ip", params.HTTPRequest.RemoteAddr)

	if status != "" {
		log.Err("Error execution command from Dispenser: ", status)
	}

	switch errors.Cause(err) {
	case nil:
		return op.NewVolumeDispenserOK().WithPayload(&op.VolumeDispenserOKBody{Status: &status, Volume: &volume})
	case app.ErrNotFound:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewVolumeDispenserNotFound().WithPayload("Volume from Arduino not found")
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewVolumeDispenserInternalServerError()
	}
}

func (svc *service) GetLevel(params op.GetLevelParams) op.GetLevelResponder {
	answer, err := svc.app.GetLevel()

	log.Info("Get water level", "IP", params.HTTPRequest.RemoteAddr)

	switch errors.Cause(err) {
	case nil:
		return op.NewGetLevelOK().WithPayload(&op.GetLevelOKBody{Level: &answer})
	case app.ErrNotFound:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetLevelNotFound().WithPayload("Level from Arduino not found")
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetLevelInternalServerError()
	}
}

func (svc *service) pressButton(params op.PressButtonParams) op.PressButtonResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		log.Info("pressButton: not found", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewPressButtonNotFound().WithPayload("station not found")
	}
	err = svc.app.PressButton(stationID, *params.Args.ButtonID)

	log.Info("pressButton", "buttonID", *params.Args.ButtonID, "stationID", stationID, "ip", params.HTTPRequest.RemoteAddr)

	switch errors.Cause(err) {
	case nil:
		return op.NewPressButtonNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewPressButtonInternalServerError()
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
		Host:           strings.TrimSpace(params.Args.Host),
		Port:           strings.TrimSpace(params.Args.Port),
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
	log.Info("cardReaderConfig", "hash", *params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	id, err := svc.getID(string(*params.Args.Hash))
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
	users, err := svc.app.Users(params.HTTPRequest.Context(), auth)
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
	return op.NewGetUserOK().WithPayload(apiUserReport(app.User{
		Login:      auth.Login,
		FirstName:  auth.FirstName,
		MiddleName: auth.MiddleName,
		LastName:   auth.LastName,
		IsAdmin:    auth.IsAdmin,
		IsOperator: auth.IsOperator,
		IsEngineer: auth.IsEngineer,
	}))
}

func (svc *service) createUser(params op.CreateUserParams, auth *app.Auth) op.CreateUserResponder {
	log.Debug("createUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	user, err := svc.app.CreateUser(params.HTTPRequest.Context(), app.UserCreation{
		Login:      string(*params.Args.Login),
		Password:   string(*params.Args.Password),
		FirstName:  (*string)(params.Args.FirstName),
		MiddleName: (*string)(params.Args.MiddleName),
		LastName:   (*string)(params.Args.LastName),
		IsAdmin:    (*bool)(params.Args.IsAdmin),
		IsEngineer: (*bool)(params.Args.IsEngineer),
		IsOperator: (*bool)(params.Args.IsOperator),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("created user", "id", user)
		return op.NewCreateUserCreated().WithPayload(&op.CreateUserCreatedBody{
			ID: newInt64(int64(user.ID)),
		})
	case app.ErrLoginNotUnique, app.ErrPasswordNotUnique:
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
	user, err := svc.app.UpdateUser(string(*params.Args.Login), app.UserUpdate{
		FirstName:  (*string)(params.Args.FirstName),
		MiddleName: (*string)(params.Args.MiddleName),
		LastName:   (*string)(params.Args.LastName),
		IsAdmin:    (*bool)(params.Args.IsAdmin),
		IsOperator: (*bool)(params.Args.IsOperator),
		IsEngineer: (*bool)(params.Args.IsEngineer),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("updated user", "id", user)
		return op.NewUpdateUserCreated().WithPayload(&op.UpdateUserCreatedBody{
			ID: newInt64(int64(user.ID)),
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
	user, err := svc.app.UpdateUserPassword(params.HTTPRequest.Context(), string(*params.Args.Login), app.UpdatePasswordData{
		OldPassword: string(*params.Args.OldPassword),
		NewPassword: string(*params.Args.NewPassword),
	}, auth)
	switch errors.Cause(err) {
	case nil:
		log.Debug("updated user", "id", user)
		return op.NewUpdateUserPasswordCreated().WithPayload(&op.UpdateUserPasswordCreatedBody{
			ID: newInt64(int64(user.ID)),
		})
	case app.ErrNotFound:
		return op.NewUpdateUserPasswordNotFound()
	case app.ErrAccessDenied, app.ErrPasswordNotUnique:
		return op.NewUpdateUserPasswordForbidden()
	default:
		return op.NewUpdateUserPasswordInternalServerError()
	}
}

func (svc *service) deleteUser(params op.DeleteUserParams, auth *app.Auth) op.DeleteUserResponder {
	log.Debug("deleteUser", "login", auth.Login, "isAdmin", auth.IsAdmin)
	err := svc.app.DeleteUser(params.HTTPRequest.Context(), string(*params.Args.Login), auth)
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

func (svc *service) stationStatCurrent(params op.StationStatCurrentParams, auth *app.Auth) op.StationStatCurrentResponder {
	var id *app.StationID
	if params.Args.StationID != nil {
		tmp := app.StationID(*params.Args.StationID)
		id = &tmp
	}
	report, err := svc.app.RelayReportCurrent(auth, id)

	switch errors.Cause(err) {
	case nil:
		return op.NewStationStatCurrentOK().WithPayload(apiStationsStat(report))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationStatCurrentInternalServerError()
	}
}

func (svc *service) stationStatDates(params op.StationStatDatesParams, auth *app.Auth) op.StationStatDatesResponder {
	log.Info("stationStatDates", "startDate", time.Unix(*params.Args.StartDate, 0), "endDate", time.Unix(*params.Args.EndDate, 0), "ip", params.HTTPRequest.RemoteAddr)
	var id *app.StationID
	if params.Args.StationID != nil {
		tmp := app.StationID(*params.Args.StationID)
		id = &tmp
	}
	report, err := svc.app.RelayReportDates(auth, id, time.Unix(*params.Args.StartDate, 0), time.Unix(*params.Args.EndDate, 0))

	switch errors.Cause(err) {
	case nil:
		return op.NewStationStatDatesOK().WithPayload(apiStationsStat(report))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewStationStatDatesInternalServerError()
	}
}

func (svc *service) resetStationStat(params op.ResetStationStatParams, auth *app.Auth) op.ResetStationStatResponder {
	err := svc.app.ResetStationStat(auth, app.StationID(*params.Args.StationID))

	switch errors.Cause(err) {
	case nil:
		return op.NewResetStationStatNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewResetStationStatInternalServerError()
	}
}

func (svc *service) addAdvertisingCampaign(params op.AddAdvertisingCampaignParams, auth *app.Auth) op.AddAdvertisingCampaignResponder {
	_, err := svc.app.AddAdvertisingCampaign(params.HTTPRequest.Context(), auth, appAdvertisingCampaign(params.Args))

	switch errors.Cause(err) {
	case nil:
		return op.NewAddAdvertisingCampaignNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAddAdvertisingCampaignInternalServerError()
	}
}

func (svc *service) editAdvertisingCampaign(params op.EditAdvertisingCampaignParams, auth *app.Auth) op.EditAdvertisingCampaignResponder {
	err := svc.app.EditAdvertisingCampaign(auth, appAdvertisingCampaign(params.Args))

	switch errors.Cause(err) {
	case nil:
		return op.NewEditAdvertisingCampaignNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewEditAdvertisingCampaignInternalServerError()
	}
}

func (svc *service) advertisingCampaign(params op.AdvertisingCampaignParams, auth *app.Auth) op.AdvertisingCampaignResponder {
	var startDate *time.Time
	if params.Args.StartDate != nil {
		start := time.Unix(*params.Args.StartDate, 0)
		startDate = &start
	}
	var endDate *time.Time
	if params.Args.EndDate != nil {
		end := time.Unix(*params.Args.EndDate, 0)
		endDate = &end
	}

	res, err := svc.app.AdvertisingCampaign(auth, startDate, endDate)

	switch errors.Cause(err) {
	case nil:
		return op.NewAdvertisingCampaignOK().WithPayload(apiAdvertisingCampaigns(res))
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAdvertisingCampaignInternalServerError()
	}
}

func (svc *service) advertisingCampaignByID(params op.AdvertisingCampaignByIDParams, auth *app.Auth) op.AdvertisingCampaignByIDResponder {
	res, err := svc.app.AdvertisingCampaignByID(auth, *params.Args.ID)

	switch errors.Cause(err) {
	case nil:
		return op.NewAdvertisingCampaignByIDOK().WithPayload(apiAdvertisingCampaign(res))
	case app.ErrNotFound:
		return op.NewAdvertisingCampaignByIDNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewAdvertisingCampaignByIDInternalServerError()
	}
}

func (svc *service) delAdvertistingCampagin(params op.DelAdvertisingCampaignParams, auth *app.Auth) op.DelAdvertisingCampaignResponder {
	err := svc.app.DelAdvertisingCampaign(auth, *params.Args.ID)

	switch errors.Cause(err) {
	case nil:
		return op.NewDelAdvertisingCampaignNoContent()
	case app.ErrAccessDenied:
		return op.NewDelAdvertisingCampaignForbidden()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewDelAdvertisingCampaignInternalServerError()
	}
}

func (svc *service) getStationDiscount(params op.GetStationDiscountsParams) op.GetStationDiscountsResponder {
	stationID, err := svc.getID(string(params.Args.Hash))
	if err != nil {
		return op.NewGetStationDiscountsNotFound()
	}

	res, err := svc.app.GetStationDiscount(stationID)

	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationDiscountsOK().WithPayload(apiStationDiscount(*res))
	default:
		return op.NewGetStationDiscountsInternalServerError()
	}
}

func (svc *service) createSession(params op.CreateSessionParams) op.CreateSessionResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		return op.NewCreateSessionNotFound()
	}

	sessionID, QR, err := svc.app.CreateSession(def.OpenwashingURL, stationID)
	if err == nil {
		return op.NewCreateSessionOK().WithPayload(apiCreateSession(sessionID, QR))
	}

	log.PrintErr(err, "stationID", stationID)
	switch errors.Cause(err) {
	case app.ErrSessionNotFound:
		return op.NewCreateSessionNotFound()
	default:
		return op.NewCreateSessionInternalServerError()
	}
}

func (svc *service) endSession(params op.EndSessionParams) op.EndSessionResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		return op.NewEndSessionNotFound()
	}

	sessionID := *params.Args.SessionID

	err = svc.app.EndSession(stationID, app.BonusSessionID(sessionID))
	if err == nil {
		return op.NewEndSessionNoContent()
	}

	log.PrintErr(err, "sessionID", sessionID)
	switch errors.Cause(err) {
	case app.ErrSessionNotFound:
		return op.NewEndSessionNotFound()
	default:
		return op.NewEndSessionInternalServerError()
	}
}

func (svc *service) setBonuses(params op.SetBonusesParams) op.SetBonusesResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		return op.NewSetBonusesNotFound()
	}

	bonusAmount := int(params.Args.Bonuses)
	err = svc.app.SetBonuses(stationID, bonusAmount)
	if err == nil {
		return op.NewSetBonusesNoContent()
	}

	log.PrintErr(err, "stationID", stationID, "Bonus amount", bonusAmount)
	switch errors.Cause(err) {
	case app.ErrUserIsNotAuthorized:
		return op.NewSetBonusesUnauthorized()
	default:
		return op.NewSetBonusesInternalServerError()
	}
}

func (svc *service) getServerInfo(params op.GetServerInfoParams) op.GetServerInfoResponder {
	info := svc.app.GetServerInfo()
	return op.NewGetServerInfoOK().WithPayload(apiGetServerInfo(info.BonusServiceURL))
}

func (svc *service) getPublicKey(params op.GetPublicKeyParams) op.GetPublicKeyResponder {
	key, err := svc.app.GetPublicKey()

	switch errors.Cause(err) {
	case nil:
		return op.NewGetPublicKeyOK().WithPayload(&model.PublicKey{PublicKey: &key})
	default:
		log.PrintErr(err)
		return op.NewGetPublicKeyInternalServerError()
	}
}

func (svc *service) getFirmwareVersions(params op.GetStationFirmwareVersionsParams, auth *app.Auth) op.GetStationFirmwareVersionsResponder {
	versions, err := svc.app.GetVersions(app.StationID(params.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationFirmwareVersionsOK().WithPayload(apiListFirmwareVersions(versions))
	case app.ErrNotFound:
		return op.NewGetStationFirmwareVersionsNotFound()
	default:
		log.PrintErr(err)
		return op.NewGetStationFirmwareVersionsInternalServerError()
	}
}

func (svc *service) getListTasks(params op.GetListTasksParams, auth *app.Auth) op.GetListTasksResponder {
	tasks, err := svc.app.GetListTasks(appTasksFilter(params))

	switch errors.Cause(err) {
	case nil:
		return op.NewGetListTasksOK().WithPayload(apiListTasks(tasks))
	default:
		log.PrintErr(err)
		return op.NewGetListTasksInternalServerError()
	}
}

func (svc *service) getTask(params op.GetTaskParams, auth *app.Auth) op.GetTaskResponder {
	task, err := svc.app.GetTask(int(params.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewGetTaskOK().WithPayload(apiTask(task))
	case app.ErrNotFound:
		return op.NewGetTaskNotFound()
	default:
		log.PrintErr(err)
		return op.NewGetTaskInternalServerError()
	}
}

func (svc *service) createTask(params op.CreateTaskParams, auth *app.Auth) op.CreateTaskResponder {
	task, err := svc.app.CreateTask(appCreateTask(*params.Args))

	switch errors.Cause(err) {
	case nil:
		return op.NewCreateTaskOK().WithPayload(apiTask(task))
	case app.ErrNotFound:
		return op.NewCreateTaskNotFound()
	case app.ErrWrongParameter:
		return op.NewCreateTaskBadRequest()
	default:
		log.PrintErr(err)
		return op.NewCreateTaskInternalServerError()
	}
}

func (svc *service) createTaskByHash(params op.CreateTaskByHashParams) op.CreateTaskByHashResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		return op.NewCreateTaskByHashNotFound()
	}

	s := int64(stationID)
	task, err := svc.app.CreateTask(appCreateTask(model.CreateTask{
		StationID: &s,
		Type:      params.Args.Type,
		VersionID: params.Args.VersionID,
	}))

	switch errors.Cause(err) {
	case nil:
		return op.NewCreateTaskByHashOK().WithPayload(apiTask(task))
	case app.ErrNotFound:
		return op.NewCreateTaskByHashNotFound()
	case app.ErrWrongParameter:
		return op.NewCreateTaskByHashBadRequest()
	default:
		log.PrintErr(err)
		return op.NewCreateTaskByHashInternalServerError()
	}
}

func (svc *service) getListBuildScripts(params op.GetListBuildScriptsParams, auth *app.Auth) op.GetListBuildScriptsResponder {
	buildScripts, err := svc.app.GetListBuildScripts()

	switch errors.Cause(err) {
	case nil:
		return op.NewGetListBuildScriptsOK().WithPayload(apiListBuildScripts(buildScripts))
	default:
		log.PrintErr(err)
		return op.NewGetListBuildScriptsInternalServerError()
	}
}

func (svc *service) getBuildScript(params op.GetBuildScriptParams, auth *app.Auth) op.GetBuildScriptResponder {
	buildScript, err := svc.app.GetBuildScript(app.StationID(params.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewGetBuildScriptOK().WithPayload(apiBuildScript(buildScript))
	case app.ErrNotFound:
		return op.NewGetBuildScriptNotFound()
	default:
		log.PrintErr(err)
		return op.NewGetBuildScriptInternalServerError()
	}
}

func (svc *service) setBuildScript(params op.SetBuildScriptParams, auth *app.Auth) op.SetBuildScriptResponder {
	buildScript, err := svc.app.SetBuildScript(appSetBuildScript(*params.Args))

	switch errors.Cause(err) {
	case nil:
		return op.NewSetBuildScriptOK().WithPayload(apiBuildScript(buildScript))
	case app.ErrNotFound:
		return op.NewSetBuildScriptNotFound()
	default:
		log.PrintErr(err)
		return op.NewSetBuildScriptInternalServerError()
	}
}

func (svc *service) deleteBuildScript(params op.DeleteBuildScriptParams, auth *app.Auth) op.DeleteBuildScriptResponder {
	err := svc.app.DeleteBuildScript(app.StationID(params.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewDeleteBuildScriptNoContent()
	case app.ErrNotFound:
		return op.NewDeleteBuildScriptNotFound()
	default:
		log.PrintErr(err)
		return op.NewDeleteBuildScriptInternalServerError()
	}
}

func (svc *service) copyFirmware(params op.FirmwareVersionsCopyParams, auth *app.Auth) op.FirmwareVersionsCopyResponder {
	err := svc.app.CopyFirmware(app.StationID(params.ID), app.StationID(params.ToID))

	switch errors.Cause(err) {
	case nil:
		return op.NewFirmwareVersionsCopyNoContent()
	case app.ErrNotFound:
		return op.NewFirmwareVersionsCopyNotFound()
	case app.ErrStationDirectoryNotExist:
		return op.NewFirmwareVersionsCopyBadRequest()
	case app.ErrTaskStarted:
		return op.NewFirmwareVersionsCopyBadRequest()
	default:
		log.PrintErr(err)
		return op.NewFirmwareVersionsCopyInternalServerError()
	}
}

func (svc *service) getVersionBuffered(params op.GetStationFirmwareVersionBufferedParams, auth *app.Auth) op.GetStationFirmwareVersionBufferedResponder {
	v, err := svc.app.GetVersionBuffered(app.StationID(params.ID))

	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationFirmwareVersionBufferedOK().WithPayload(apiFirmwareVersion(&v))
	case app.ErrNotFound:
		return op.NewGetStationFirmwareVersionBufferedNotFound()
	default:
		log.PrintErr(err)
		return op.NewGetStationFirmwareVersionBufferedInternalServerError()
	}
}

func (svc *service) addLog(params op.AddLogParams) op.AddLogResponder {
	stationID, err := svc.getID(string(*params.Args.Hash))
	if err != nil {
		return op.NewAddLogNotFound()
	}

	_, err = svc.app.AddOpenwashingLog(openwashingLogCreateToApp(stationID, *params.Args))

	switch errors.Cause(err) {
	case nil:
		return op.NewAddLogNoContent()
	default:
		log.PrintErr(err)
		return op.NewAddLogInternalServerError()
	}
}
