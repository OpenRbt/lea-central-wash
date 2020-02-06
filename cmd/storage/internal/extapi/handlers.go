package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

func (svc *service) load(params op.LoadParams) op.LoadResponder {
	log.Info("load", "hash", params.Args.Hash, "key", *params.Args.Key, "ip", params.HTTPRequest.RemoteAddr)
	value, err := svc.app.Load(string(params.Args.Hash), *params.Args.Key)
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

func (svc *service) save(params op.SaveParams) op.SaveResponder {
	log.Info("save", "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
	err := svc.app.Save(string(params.Args.Hash), *params.Args.KeyPair.Key, []byte(*params.Args.KeyPair.Value))
	switch errors.Cause(err) {
	case nil:
		return op.NewSaveNoContent()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "key", *params.Args.KeyPair.Key, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSaveInternalServerError()
	}
}

func (svc *service) ping(params op.PingParams) op.PingResponder {
	log.Info("ping", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	// for test
	if params.Args.Hash == "give me money" {
		return op.NewPingOK().WithPayload(&op.PingOKBody{
			ServiceAmount: 10,
		})
	}
	return op.NewPingOK()
}

func (svc *service) info(params op.InfoParams) op.InfoResponder {
	return op.NewInfoOK().WithPayload(svc.app.Info())
}
