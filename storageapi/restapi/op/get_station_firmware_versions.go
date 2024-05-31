// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// GetStationFirmwareVersionsHandlerFunc turns a function with the right signature into a get station firmware versions handler
type GetStationFirmwareVersionsHandlerFunc func(GetStationFirmwareVersionsParams, *storageapi.Profile) GetStationFirmwareVersionsResponder

// Handle executing the request and returning a response
func (fn GetStationFirmwareVersionsHandlerFunc) Handle(params GetStationFirmwareVersionsParams, principal *storageapi.Profile) GetStationFirmwareVersionsResponder {
	return fn(params, principal)
}

// GetStationFirmwareVersionsHandler interface for that can handle valid get station firmware versions params
type GetStationFirmwareVersionsHandler interface {
	Handle(GetStationFirmwareVersionsParams, *storageapi.Profile) GetStationFirmwareVersionsResponder
}

// NewGetStationFirmwareVersions creates a new http.Handler for the get station firmware versions operation
func NewGetStationFirmwareVersions(ctx *middleware.Context, handler GetStationFirmwareVersionsHandler) *GetStationFirmwareVersions {
	return &GetStationFirmwareVersions{Context: ctx, Handler: handler}
}

/*
	GetStationFirmwareVersions swagger:route GET /stations/{id}/firmware-versions getStationFirmwareVersions

GetStationFirmwareVersions get station firmware versions API
*/
type GetStationFirmwareVersions struct {
	Context *middleware.Context
	Handler GetStationFirmwareVersionsHandler
}

func (o *GetStationFirmwareVersions) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetStationFirmwareVersionsParams()
	uprinc, aCtx, err := o.Context.Authorize(r, route)
	if err != nil {
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}
	if aCtx != nil {
		*r = *aCtx
	}
	var principal *storageapi.Profile
	if uprinc != nil {
		principal = uprinc.(*storageapi.Profile) // this is really a storageapi.Profile, I promise
	}

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params, principal) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}