package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

func (svc *service) load(params op.LoadParams) op.LoadResponder {
	log.Info("load", "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	value, err := svc.app.Load(string(params.Args.StationID), *params.Args.Key)
	switch errors.Cause(err) {
	case nil:
		return op.NewLoadOK().WithPayload(string(value))
	case app.ErrNotFound:
		log.Info("load: not found", "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadNotFound()
	default:
		log.PrintErr(err, "stationID", params.Args.StationID, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadInternalServerError()
	}
}

func (svc *service) save(params op.SaveParams) op.SaveResponder {
	log.Info("save", "stationID", params.Args.StationID, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	err := svc.app.Save(string(params.Args.StationID), *params.Args.KeyPair.Key, []byte(*params.Args.KeyPair.Value))
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveNoContent()
	default:
		log.PrintErr(err, "stationID", params.Args.StationID, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveInternalServerError()
	}
}

func (svc *service) ping(params op.PingParams) op.PingResponder {
	log.Info("ping", "ip", params.HTTPRequest.RemoteAddr)
	return op.NewPingOK()
}
