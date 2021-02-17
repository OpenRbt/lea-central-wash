package dal

import (
	"encoding/json"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

func appSetUsers(v []resUser) []app.UserData {
	var res []app.UserData
	for i := range v {
		res = append(res, app.UserData{
			ID:         v[i].ID,
			Login:      v[i].Login,
			Password:   v[i].Password,
			FirstName:  &v[i].FirstName,
			MiddleName: &v[i].MiddleName,
			LastName:   &v[i].LastName,
			IsAdmin:    &v[i].IsAdmin,
			IsOperator: &v[i].IsOperator,
			IsEngineer: &v[i].IsEngineer,
		})
	}
	return res
}

func appSetStation(v []resStation) []app.SetStation {
	var res []app.SetStation
	for i := range v {
		res = append(res, app.SetStation{
			ID:           v[i].ID,
			Name:         v[i].Name,
			PreflightSec: v[i].PreflightSec,
		})
	}
	return res
}

func appStationsVariables(v []resStationsVariables) []app.StationsVariables {
	var res []app.StationsVariables
	id := app.StationID(-1)
	count := -1
	for i := range v {
		if id != v[i].ID {
			res = append(res, app.StationsVariables{
				ID:      v[i].ID,
				Name:    v[i].Name,
				KeyPair: []app.KeyPair{},
			})
			id = v[i].ID
			count++
		}
		res[count].KeyPair = append(res[count].KeyPair, app.KeyPair{
			Key:   v[i].Key,
			Value: v[i].Value,
		})
	}
	return res
}

func appPrograms(p []resPrograms) (res []app.Program) {
	for i := range p {
		res = append(res, app.Program{
			ID:               p[i].ID,
			Name:             p[i].Name,
			Price:            p[i].Price,
			PreflightEnabled: p[i].PreflightEnabled,
			Relays:           appProgramRelays(p[i].Relays),
			PreflightRelays:  appProgramRelays(p[i].PreflightRelays),
		})
	}
	return res
}

func appProgramRelays(jsonRelays string) (res []app.Relay) {
	err := json.Unmarshal([]byte(jsonRelays), &res)

	if err != nil {
		panic(err)
	}
	return res
}

func dalProgramRelays(relays []app.Relay) (jsonRelays string) {
	bytes, err := json.Marshal(relays)

	if err != nil {
		panic(err)
	}

	jsonRelays = string(bytes)
	return jsonRelays
}

func appKasse(k resKasse) (res app.Kasse) {
	res.CashierFullName = k.CashierFullName
	res.CashierINN = k.CashierINN
	res.ReceiptItem = k.ReceiptItem
	res.TaxType = k.TaxType

	return res
}

func appStationProgram(p []resStationProgram) (res []app.StationProgram) {
	for i := range p {
		res = append(res, app.StationProgram{
			ButtonID:  p[i].ButtonID,
			ProgramID: p[i].ProgramID,
		})
	}
	return res
}

func appStationConfig(p []resStationConfig) (res app.StationConfig) {
	if len(p) == 0 {
		return
	}
	res.ID = p[0].ID
	res.Name = p[0].Name
	res.PreflightSec = p[0].PreflightSec

	for i := range p {
		res.Programs = append(res.Programs, app.Program{
			ID:               p[i].ProgramID,
			ButtonID:         p[i].ButtonID,
			Name:             p[i].ProgramName,
			Price:            p[i].Price,
			PreflightEnabled: p[i].PreflightEnabled,
			Relays:           appProgramRelays(p[i].Relays),
			PreflightRelays:  appProgramRelays(p[i].PreflightRelays),
		})
	}
	return res
}
