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

func (svc *service) setStationConfigVarInt(params op.SetStationConfigVarIntParams, auth *app.Auth) op.SetStationConfigVarIntResponder {
	err := svc.app.SetStationConfigInt(auth, appStationConfigInt(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetStationConfigVarIntNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationConfigVarIntInternalServerError()
	}
}

func (svc *service) setStationConfigVarBool(params op.SetStationConfigVarBoolParams, auth *app.Auth) op.SetStationConfigVarBoolResponder {
	err := svc.app.SetStationConfigBool(auth, appStationConfigBool(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetStationConfigVarBoolNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationConfigVarBoolInternalServerError()
	}
}

func (svc *service) setStationConfigVarString(params op.SetStationConfigVarStringParams, auth *app.Auth) op.SetStationConfigVarStringResponder {
	err := svc.app.SetStationConfigString(auth, appStationConfigString(params.Args))
	switch errors.Cause(err) {
	case nil:
		return op.NewSetStationConfigVarStringNoContent()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationConfigVarStringInternalServerError()
	}
}

func (svc *service) getStationConfigVarBool(params op.GetStationConfigVarBoolParams, auth *app.Auth) op.GetStationConfigVarBoolResponder {

	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationConfigVarBool: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarBoolNotFound()
	}

	res, err := svc.app.GetStationConfigBool(auth, params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationConfigVarBoolOK().WithPayload(apiStationConfigBool(res))
	case app.ErrNotFound:
		return op.NewGetStationConfigVarBoolNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarBoolInternalServerError()
	}
}

func (svc *service) getStationConfigVarInt(params op.GetStationConfigVarIntParams, auth *app.Auth) op.GetStationConfigVarIntResponder {

	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationConfigVarInt: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarIntNotFound()
	}

	res, err := svc.app.GetStationConfigInt(auth, params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationConfigVarIntOK().WithPayload(apiStationConfigInt(res))
	case app.ErrNotFound:
		return op.NewGetStationConfigVarIntNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarIntInternalServerError()
	}
}

func (svc *service) getStationConfigVarString(params op.GetStationConfigVarStringParams, auth *app.Auth) op.GetStationConfigVarStringResponder {

	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationConfigVarString: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarStringNotFound()
	}

	res, err := svc.app.GetStationConfigString(auth, params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationConfigVarStringOK().WithPayload(apiStationConfigString(res))
	case app.ErrNotFound:
		return op.NewGetStationConfigVarStringNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationConfigVarStringInternalServerError()
	}
}
