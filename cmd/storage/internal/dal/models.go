package dal

import (
	"encoding/json"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

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
			ID:   p[i].ID,
			Name: p[i].Name,
		})
	}
	return res
}

func appUnPackProgramRelays(jsonRelays string) (res []app.Relay, err error) {

	err = json.Unmarshal([]byte(jsonRelays), &res)

	return res, err
}

func appPackProgramRelays(relays []app.Relay) (jsonRelays string, err error) {
	var bytes []byte
	bytes, err = json.Marshal(relays)

	jsonRelays = string(bytes)

	return jsonRelays, err
}
