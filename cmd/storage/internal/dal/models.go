package dal

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

func appSetStation(v []resStation) []app.SetStation {
	var res []app.SetStation
	for i := range v {
		hash := ""
		if v[i].Hash != nil {
			hash = *v[i].Hash
		}
		res = append(res, app.SetStation{
			Hash: hash,
			ID:   v[i].ID,
			Name: v[i].Name,
		})
	}
	return res
}

func appStationsKeyPair(v []resStationKeyPair) []app.StationKeyPair {
	var res []app.StationKeyPair
	id := -1
	count := -1
	for i := range v {
		if id != v[i].ID {
			res = append(res, app.StationKeyPair{
				Hash:    v[i].Hash,
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
