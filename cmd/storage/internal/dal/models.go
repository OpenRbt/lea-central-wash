package dal

import (
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
