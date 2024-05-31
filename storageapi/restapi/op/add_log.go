// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"
)

// AddLogHandlerFunc turns a function with the right signature into a add log handler
type AddLogHandlerFunc func(AddLogParams) AddLogResponder

// Handle executing the request and returning a response
func (fn AddLogHandlerFunc) Handle(params AddLogParams) AddLogResponder {
	return fn(params)
}

// AddLogHandler interface for that can handle valid add log params
type AddLogHandler interface {
	Handle(AddLogParams) AddLogResponder
}

// NewAddLog creates a new http.Handler for the add log operation
func NewAddLog(ctx *middleware.Context, handler AddLogHandler) *AddLog {
	return &AddLog{Context: ctx, Handler: handler}
}

/*
	AddLog swagger:route POST /add-log addLog

AddLog add log API
*/
type AddLog struct {
	Context *middleware.Context
	Handler AddLogHandler
}

func (o *AddLog) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewAddLogParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}