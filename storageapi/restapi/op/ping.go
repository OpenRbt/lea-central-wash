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

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// PingHandlerFunc turns a function with the right signature into a ping handler
type PingHandlerFunc func(PingParams) PingResponder

// Handle executing the request and returning a response
func (fn PingHandlerFunc) Handle(params PingParams) PingResponder {
	return fn(params)
}

// PingHandler interface for that can handle valid ping params
type PingHandler interface {
	Handle(PingParams) PingResponder
}

// NewPing creates a new http.Handler for the ping operation
func NewPing(ctx *middleware.Context, handler PingHandler) *Ping {
	return &Ping{Context: ctx, Handler: handler}
}

/* Ping swagger:route POST /ping ping

Ping ping API

*/
type Ping struct {
	Context *middleware.Context
	Handler PingHandler
}

func (o *Ping) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewPingParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// PingBody ping body
//
// swagger:model PingBody
type PingBody struct {

	// current balance
	CurrentBalance int64 `json:"currentBalance,omitempty"`

	// current program
	CurrentProgram int64 `json:"currentProgram,omitempty"`

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// Validate validates this ping body
func (o *PingBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PingBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if o.Hash != nil {
		if err := o.Hash.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this ping body based on the context it is used
func (o *PingBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PingBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {
		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *PingBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *PingBody) UnmarshalBinary(b []byte) error {
	var res PingBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// PingOKBody ping o k body
//
// swagger:model PingOKBody
type PingOKBody struct {

	// button ID
	ButtonID int64 `json:"ButtonID,omitempty"`

	// last discount update
	LastDiscountUpdate int64 `json:"lastDiscountUpdate,omitempty"`

	// last update
	LastUpdate int64 `json:"lastUpdate,omitempty"`

	// open station
	// Required: true
	OpenStation *bool `json:"openStation"`

	// service amount
	// Required: true
	ServiceAmount *int64 `json:"serviceAmount"`
}

// Validate validates this ping o k body
func (o *PingOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateOpenStation(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateServiceAmount(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PingOKBody) validateOpenStation(formats strfmt.Registry) error {

	if err := validate.Required("pingOK"+"."+"openStation", "body", o.OpenStation); err != nil {
		return err
	}

	return nil
}

func (o *PingOKBody) validateServiceAmount(formats strfmt.Registry) error {

	if err := validate.Required("pingOK"+"."+"serviceAmount", "body", o.ServiceAmount); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this ping o k body based on context it is used
func (o *PingOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *PingOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *PingOKBody) UnmarshalBinary(b []byte) error {
	var res PingOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
