package dal

import (
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
			ID:   v[i].ID,
			Name: v[i].Name,
		})
	}
	return res
}
