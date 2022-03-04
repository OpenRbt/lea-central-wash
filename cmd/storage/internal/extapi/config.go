package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/pkg/errors"
)

func (svc *service) setConfigVarInt(params op.SetConfigVarIntParams, auth *app.Auth) op.SetConfigVarIntResponder {
	err := svc.app.SetConfigInt(auth, appConfigInt(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetConfigVarIntNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetConfigVarIntInternalServerError()
	}
}

func (svc *service) setConfigVarBool(params op.SetConfigVarBoolParams, auth *app.Auth) op.SetConfigVarBoolResponder {
	err := svc.app.SetConfigBool(auth, appConfigBool(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetConfigVarBoolNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetConfigVarBoolInternalServerError()
	}
}

func (svc *service) setConfigVarString(params op.SetConfigVarStringParams, auth *app.Auth) op.SetConfigVarStringResponder {
	err := svc.app.SetConfigString(auth, appConfigString(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetConfigVarStringNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetConfigVarStringInternalServerError()
	}
}

func (svc *service) getConfigVarBool(params op.GetConfigVarBoolParams, auth *app.Auth) op.GetConfigVarBoolResponder {
	res, err := svc.app.GetConfigBool(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetConfigVarBoolOK().WithPayload(apiConfigBool(res))
	case app.ErrNotFound:
		return op.NewGetConfigVarBoolNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetConfigVarBoolInternalServerError()
	}
}

func (svc *service) getConfigVarInt(params op.GetConfigVarIntParams, auth *app.Auth) op.GetConfigVarIntResponder {
	res, err := svc.app.GetConfigInt(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetConfigVarIntOK().WithPayload(apiConfigInt(res))
	case app.ErrNotFound:
		return op.NewGetConfigVarIntNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetConfigVarIntInternalServerError()
	}
}

func (svc *service) getConfigVarString(params op.GetConfigVarStringParams, auth *app.Auth) op.GetConfigVarStringResponder {
	res, err := svc.app.GetConfigString(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetConfigVarStringOK().WithPayload(apiConfigString(res))
	case app.ErrNotFound:
		return op.NewGetConfigVarStringNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetConfigVarStringInternalServerError()
	}
}
