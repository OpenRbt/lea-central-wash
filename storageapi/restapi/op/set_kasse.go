// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	middleware "github.com/go-openapi/runtime/middleware"
)

// SetKasseHandlerFunc turns a function with the right signature into a set kasse handler
type SetKasseHandlerFunc func(SetKasseParams) SetKasseResponder

// Handle executing the request and returning a response
func (fn SetKasseHandlerFunc) Handle(params SetKasseParams) SetKasseResponder {
	return fn(params)
}

// SetKasseHandler interface for that can handle valid set kasse params
type SetKasseHandler interface {
	Handle(SetKasseParams) SetKasseResponder
}

// NewSetKasse creates a new http.Handler for the set kasse operation
func NewSetKasse(ctx *middleware.Context, handler SetKasseHandler) *SetKasse {
	return &SetKasse{Context: ctx, Handler: handler}
}

/*SetKasse swagger:route POST /set-kasse setKasse

SetKasse set kasse API

*/
type SetKasse struct {
	Context *middleware.Context
	Handler SetKasseHandler
}

func (o *SetKasse) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewSetKasseParams()

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request

	o.Context.Respond(rw, r, route.Produces, route, res)

}
