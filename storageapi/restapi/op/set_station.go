// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"
)

// SetStationHandlerFunc turns a function with the right signature into a set station handler
type SetStationHandlerFunc func(SetStationParams) SetStationResponder

// Handle executing the request and returning a response
func (fn SetStationHandlerFunc) Handle(params SetStationParams) SetStationResponder {
	return fn(params)
}

// SetStationHandler interface for that can handle valid set station params
type SetStationHandler interface {
	Handle(SetStationParams) SetStationResponder
}

// NewSetStation creates a new http.Handler for the set station operation
func NewSetStation(ctx *middleware.Context, handler SetStationHandler) *SetStation {
	return &SetStation{Context: ctx, Handler: handler}
}

/*
	SetStation swagger:route POST /set-station setStation

SetStation set station API
*/
type SetStation struct {
	Context *middleware.Context
	Handler SetStationHandler
}

func (o *SetStation) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetStationParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}
