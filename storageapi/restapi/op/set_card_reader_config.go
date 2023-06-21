// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"
)

// SetCardReaderConfigHandlerFunc turns a function with the right signature into a set card reader config handler
type SetCardReaderConfigHandlerFunc func(SetCardReaderConfigParams) SetCardReaderConfigResponder

// Handle executing the request and returning a response
func (fn SetCardReaderConfigHandlerFunc) Handle(params SetCardReaderConfigParams) SetCardReaderConfigResponder {
	return fn(params)
}

// SetCardReaderConfigHandler interface for that can handle valid set card reader config params
type SetCardReaderConfigHandler interface {
	Handle(SetCardReaderConfigParams) SetCardReaderConfigResponder
}

// NewSetCardReaderConfig creates a new http.Handler for the set card reader config operation
func NewSetCardReaderConfig(ctx *middleware.Context, handler SetCardReaderConfigHandler) *SetCardReaderConfig {
	return &SetCardReaderConfig{Context: ctx, Handler: handler}
}

/* SetCardReaderConfig swagger:route POST /set-card-reader-config setCardReaderConfig

SetCardReaderConfig set card reader config API

*/
type SetCardReaderConfig struct {
	Context *middleware.Context
	Handler SetCardReaderConfigHandler
}

func (o *SetCardReaderConfig) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetCardReaderConfigParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}
