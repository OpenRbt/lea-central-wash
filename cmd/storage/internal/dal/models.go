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
			FirstName:  v[i].FirstName,
			MiddleName: v[i].MiddleName,
			LastName:   v[i].LastName,
			Password:   v[i].Password,
			IsAdmin:    v[i].IsAdmin,
			IsOperator: v[i].IsOperator,
			IsEngineer: v[i].IsEngineer,
		})
	}
	return res
}

func appSetStation(v []resStation) []app.SetStation {
	var res []app.SetStation
	for i := range v {
		res = append(res, app.SetStation{
			ID:   v[i].ID,
			Name: v[i].Name,
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
			ID:   p[i].ProgramID,
			Name: p[i].Name,
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
