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
	case app.ErrNotFound:
		return op.NewSetStationConfigVarIntNotFound()
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
	case app.ErrNotFound:
		return op.NewSetStationConfigVarBoolNotFound()
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
	case app.ErrNotFound:
		return op.NewSetStationConfigVarStringNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewSetStationConfigVarStringInternalServerError()
	}
}

func (svc *service) getStationConfigVarBool(params op.GetStationConfigVarBoolParams, auth *app.Auth) op.GetStationConfigVarBoolResponder {
	res, err := svc.app.GetStationConfigBool(*params.Args.Name, app.StationID(*params.Args.StationID))
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
	res, err := svc.app.GetStationConfigInt(*params.Args.Name, app.StationID(*params.Args.StationID))
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
	res, err := svc.app.GetStationConfigString(*params.Args.Name, app.StationID(*params.Args.StationID))
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

func (svc *service) getStationWashConfigVarInt(params op.GetStationWashConfigVarIntParams) op.GetStationWashConfigVarIntResponder {
	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationWashConfigVarInt: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarIntNotFound()
	}

	res, err := svc.app.GetStationConfigInt(*params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationWashConfigVarIntOK().WithPayload(apiStationConfigInt(res))
	case app.ErrNotFound:
		return op.NewGetStationWashConfigVarIntNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarIntInternalServerError()
	}
}

func (svc *service) getStationWashConfigVarBool(params op.GetStationWashConfigVarBoolParams) op.GetStationWashConfigVarBoolResponder {
	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationWashConfigVarBool: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarBoolNotFound()
	}

	res, err := svc.app.GetStationConfigBool(*params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationWashConfigVarBoolOK().WithPayload(apiStationConfigBool(res))
	case app.ErrNotFound:
		return op.NewGetStationWashConfigVarBoolNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarBoolInternalServerError()
	}
}

func (svc *service) getStationWashConfigVarString(params op.GetStationWashConfigVarStringParams) op.GetStationWashConfigVarStringResponder {
	stationID, err := svc.getID(params.Args.Hash)
	if err != nil {
		log.Info("getStationWashConfigVarString: not found", "hash", params.Args.Hash, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarStringNotFound()
	}

	res, err := svc.app.GetStationConfigString(*params.Args.Name, stationID)
	switch errors.Cause(err) {
	case nil:
		return op.NewGetStationWashConfigVarStringOK().WithPayload(apiStationConfigString(res))
	case app.ErrNotFound:
		return op.NewGetStationWashConfigVarStringNotFound()
	default:
		log.PrintErr(err, "ip", params.HTTPRequest.RemoteAddr)
		return op.NewGetStationWashConfigVarStringInternalServerError()
	}
}
