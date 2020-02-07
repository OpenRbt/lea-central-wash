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

func (svc *service) loadRelay(params op.LoadRelayParams) op.LoadRelayResponder {
	log.Info("load relay", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	err := app.ErrNotFound
	if params.Args.Hash == "give me report" {
		err = nil
	}
	switch errors.Cause(err) {
	case nil:
		return op.NewLoadRelayOK().WithPayload(apiRelayReport(params.Args.Hash))
	case app.ErrNotFound:
		log.Info("load relay: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadRelayNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadRelayInternalServerError()
	}
}

func (svc *service) saveRelay(params op.SaveRelayParams) op.SaveRelayResponder {
	log.Info("save relay", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	return op.NewSaveRelayNoContent()
}

func (svc *service) loadMoney(params op.LoadMoneyParams) op.LoadMoneyResponder {
	log.Info("load money", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	err := app.ErrNotFound
	if params.Args.Hash == "give me report" {
		err = nil
	}
	switch errors.Cause(err) {
	case nil:
		return op.NewLoadMoneyOK().WithPayload(apiMoneyReport(params.Args.Hash))
	case app.ErrNotFound:
		log.Info("load money: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadMoneyNotFound()
	default:
		log.PrintErr(err, "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewLoadMoneyInternalServerError()
	}
}

func (svc *service) saveMoney(params op.SaveMoneyParams) op.SaveMoneyResponder {
	log.Info("save money", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
	return op.NewSaveMoneyNoContent()
}

func (svc *service) ping(params op.PingParams) op.PingResponder {
	log.Info("ping", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)

	// for test
	if params.Args.Hash == "give me money" {
		return op.NewPingOK().WithPayload(&op.PingOKBody{
			ServiceAmount: newInt64(10),
		})
	}
	return op.NewPingOK().WithPayload(&op.PingOKBody{
		ServiceAmount: newInt64(0),
	})
}

func (svc *service) info(params op.InfoParams) op.InfoResponder {
	return op.NewInfoOK().WithPayload(svc.app.Info())
}

func newInt64(v int64) *int64 {
	return &v
}
