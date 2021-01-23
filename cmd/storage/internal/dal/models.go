package dal

import (
	"encoding/json"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

func appSetStation(v []resStation) []app.SetStationParams {
	var res []app.SetStationParams
	for i := range v {
	hash := ""
	if v[i].Hash != nil {
		hash = *v[i].Hash
	}
	res = append(res, app.SetStationParams{
		Hash: hash,
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
