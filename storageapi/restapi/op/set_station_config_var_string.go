// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// SetStationConfigVarStringHandlerFunc turns a function with the right signature into a set station config var string handler
type SetStationConfigVarStringHandlerFunc func(SetStationConfigVarStringParams, *storageapi.Profile) SetStationConfigVarStringResponder

// Handle executing the request and returning a response
func (fn SetStationConfigVarStringHandlerFunc) Handle(params SetStationConfigVarStringParams, principal *storageapi.Profile) SetStationConfigVarStringResponder {
	return fn(params, principal)
}

// SetStationConfigVarStringHandler interface for that can handle valid set station config var string params
type SetStationConfigVarStringHandler interface {
	Handle(SetStationConfigVarStringParams, *storageapi.Profile) SetStationConfigVarStringResponder
}

// NewSetStationConfigVarString creates a new http.Handler for the set station config var string operation
func NewSetStationConfigVarString(ctx *middleware.Context, handler SetStationConfigVarStringHandler) *SetStationConfigVarString {
	return &SetStationConfigVarString{Context: ctx, Handler: handler}
}

/*
	SetStationConfigVarString swagger:route POST /set-station-config-var-string setStationConfigVarString

SetStationConfigVarString set station config var string API
*/
type SetStationConfigVarString struct {
	Context *middleware.Context
	Handler SetStationConfigVarStringHandler
}

func (o *SetStationConfigVarString) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetStationConfigVarStringParams()
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
