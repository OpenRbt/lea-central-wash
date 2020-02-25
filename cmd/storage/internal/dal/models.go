package dal

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

func appSetStation(v []resStation) []app.SetStation {
	var res []app.SetStation
	for i, _ := range v {
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
