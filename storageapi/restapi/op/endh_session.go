// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"context"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// EndhSessionHandlerFunc turns a function with the right signature into a endh session handler
type EndhSessionHandlerFunc func(EndhSessionParams) EndhSessionResponder

// Handle executing the request and returning a response
func (fn EndhSessionHandlerFunc) Handle(params EndhSessionParams) EndhSessionResponder {
	return fn(params)
}

// EndhSessionHandler interface for that can handle valid endh session params
type EndhSessionHandler interface {
	Handle(EndhSessionParams) EndhSessionResponder
}

// NewEndhSession creates a new http.Handler for the endh session operation
func NewEndhSession(ctx *middleware.Context, handler EndhSessionHandler) *EndhSession {
	return &EndhSession{Context: ctx, Handler: handler}
}

/*
	EndhSession swagger:route POST /end-session endhSession

EndhSession endh session API
*/
type EndhSession struct {
	Context *middleware.Context
	Handler EndhSessionHandler
}

func (o *EndhSession) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewEndhSessionParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// EndhSessionBody ArgEndSession
//
// swagger:model EndhSessionBody
type EndhSessionBody struct {

	// key
	// Required: true
	Key *string `json:"key"`
}

// Validate validates this endh session body
func (o *EndhSessionBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateKey(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *EndhSessionBody) validateKey(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"key", "body", o.Key); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this endh session body based on context it is used
func (o *EndhSessionBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *EndhSessionBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *EndhSessionBody) UnmarshalBinary(b []byte) error {
	var res EndhSessionBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
