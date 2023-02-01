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

func (svc *service) setLocalConfigVarInt(params op.SetLocalConfigVarIntParams, auth *app.Auth) op.SetLocalConfigVarIntResponder {
	err := svc.app.SetLocalConfigInt(auth, appLocalConfigInt(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetLocalConfigVarIntNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetLocalConfigVarIntInternalServerError()
	}
}

func (svc *service) setLocalConfigVarBool(params op.SetLocalConfigVarBoolParams, auth *app.Auth) op.SetLocalConfigVarBoolResponder {
	err := svc.app.SetLocalConfigBool(auth, appLocalConfigBool(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetLocalConfigVarBoolNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetLocalConfigVarBoolInternalServerError()
	}
}

func (svc *service) setLocalConfigVarString(params op.SetLocalConfigVarStringParams, auth *app.Auth) op.SetLocalConfigVarStringResponder {
	err := svc.app.SetLocalConfigString(auth, appLocalConfigString(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetLocalConfigVarStringNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetLocalConfigVarStringInternalServerError()
	}
}

func (svc *service) getLocalConfigVarBool(params op.GetLocalConfigVarBoolParams, auth *app.Auth) op.GetLocalConfigVarBoolResponder {
	res, err := svc.app.GetLocalConfigBool(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetLocalConfigVarBoolOK().WithPayload(apiLocalConfigBool(res))
	case app.ErrNotFound:
		return op.NewGetLocalConfigVarBoolNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetLocalConfigVarBoolInternalServerError()
	}
}

func (svc *service) getLocalConfigVarInt(params op.GetLocalConfigVarIntParams, auth *app.Auth) op.GetLocalConfigVarIntResponder {
	res, err := svc.app.GetLocalConfigInt(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetLocalConfigVarIntOK().WithPayload(apiLocalConfigInt(res))
	case app.ErrNotFound:
		return op.NewGetLocalConfigVarIntNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetLocalConfigVarIntInternalServerError()
	}
}

func (svc *service) getLocalConfigVarString(params op.GetLocalConfigVarStringParams, auth *app.Auth) op.GetLocalConfigVarStringResponder {
	res, err := svc.app.GetLocalConfigString(auth, params.Args.Name)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetLocalConfigVarStringOK().WithPayload(apiLocalConfigString(res))
	case app.ErrNotFound:
		return op.NewGetLocalConfigVarStringNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetLocalConfigVarStringInternalServerError()
	}
}
