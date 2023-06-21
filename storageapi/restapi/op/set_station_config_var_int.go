// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
)

// SetStationConfigVarIntHandlerFunc turns a function with the right signature into a set station config var int handler
type SetStationConfigVarIntHandlerFunc func(SetStationConfigVarIntParams, *storageapi.Profile) SetStationConfigVarIntResponder

// Handle executing the request and returning a response
func (fn SetStationConfigVarIntHandlerFunc) Handle(params SetStationConfigVarIntParams, principal *storageapi.Profile) SetStationConfigVarIntResponder {
	return fn(params, principal)
}

// SetStationConfigVarIntHandler interface for that can handle valid set station config var int params
type SetStationConfigVarIntHandler interface {
	Handle(SetStationConfigVarIntParams, *storageapi.Profile) SetStationConfigVarIntResponder
}

// NewSetStationConfigVarInt creates a new http.Handler for the set station config var int operation
func NewSetStationConfigVarInt(ctx *middleware.Context, handler SetStationConfigVarIntHandler) *SetStationConfigVarInt {
	return &SetStationConfigVarInt{Context: ctx, Handler: handler}
}

/* SetStationConfigVarInt swagger:route POST /set-station-config-var-int setStationConfigVarInt

SetStationConfigVarInt set station config var int API

*/
type SetStationConfigVarInt struct {
	Context *middleware.Context
	Handler SetStationConfigVarIntHandler
}

func (o *SetStationConfigVarInt) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetStationConfigVarIntParams()
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
