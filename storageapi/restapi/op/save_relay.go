// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"
)

// SaveRelayHandlerFunc turns a function with the right signature into a save relay handler
type SaveRelayHandlerFunc func(SaveRelayParams) SaveRelayResponder

// Handle executing the request and returning a response
func (fn SaveRelayHandlerFunc) Handle(params SaveRelayParams) SaveRelayResponder {
	return fn(params)
}

// SaveRelayHandler interface for that can handle valid save relay params
type SaveRelayHandler interface {
	Handle(SaveRelayParams) SaveRelayResponder
}

// NewSaveRelay creates a new http.Handler for the save relay operation
func NewSaveRelay(ctx *middleware.Context, handler SaveRelayHandler) *SaveRelay {
	return &SaveRelay{Context: ctx, Handler: handler}
}

/* SaveRelay swagger:route POST /save-relay saveRelay

SaveRelay save relay API

*/
type SaveRelay struct {
	Context *middleware.Context
	Handler SaveRelayHandler
}

func (o *SaveRelay) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSaveRelayParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}
