// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	middleware "github.com/go-openapi/runtime/middleware"
)

// StationsVariablesHandlerFunc turns a function with the right signature into a stations variables handler
type StationsVariablesHandlerFunc func(StationsVariablesParams) StationsVariablesResponder

// Handle executing the request and returning a response
func (fn StationsVariablesHandlerFunc) Handle(params StationsVariablesParams) StationsVariablesResponder {
	return fn(params)
}

// StationsVariablesHandler interface for that can handle valid stations variables params
type StationsVariablesHandler interface {
	Handle(StationsVariablesParams) StationsVariablesResponder
}

// NewStationsVariables creates a new http.Handler for the stations variables operation
func NewStationsVariables(ctx *middleware.Context, handler StationsVariablesHandler) *StationsVariables {
	return &StationsVariables{Context: ctx, Handler: handler}
}

/*StationsVariables swagger:route POST /stations-variables stationsVariables

StationsVariables stations variables API

*/
type StationsVariables struct {
	Context *middleware.Context
	Handler StationsVariablesHandler
}

func (o *StationsVariables) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewStationsVariablesParams()

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request

	o.Context.Respond(rw, r, route.Produces, route, res)

}
