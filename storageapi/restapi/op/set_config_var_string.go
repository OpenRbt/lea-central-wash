// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// SetConfigVarStringHandlerFunc turns a function with the right signature into a set config var string handler
type SetConfigVarStringHandlerFunc func(SetConfigVarStringParams, *storageapi.Profile) SetConfigVarStringResponder

// Handle executing the request and returning a response
func (fn SetConfigVarStringHandlerFunc) Handle(params SetConfigVarStringParams, principal *storageapi.Profile) SetConfigVarStringResponder {
	return fn(params, principal)
}

// SetConfigVarStringHandler interface for that can handle valid set config var string params
type SetConfigVarStringHandler interface {
	Handle(SetConfigVarStringParams, *storageapi.Profile) SetConfigVarStringResponder
}

// NewSetConfigVarString creates a new http.Handler for the set config var string operation
func NewSetConfigVarString(ctx *middleware.Context, handler SetConfigVarStringHandler) *SetConfigVarString {
	return &SetConfigVarString{Context: ctx, Handler: handler}
}

/*
	SetConfigVarString swagger:route POST /set-config-var-string setConfigVarString

SetConfigVarString set config var string API
*/
type SetConfigVarString struct {
	Context *middleware.Context
	Handler SetConfigVarStringHandler
}

func (o *SetConfigVarString) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetConfigVarStringParams()
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
