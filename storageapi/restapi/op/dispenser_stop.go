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

// DispenserStopHandlerFunc turns a function with the right signature into a dispenser stop handler
type DispenserStopHandlerFunc func(DispenserStopParams) DispenserStopResponder

// Handle executing the request and returning a response
func (fn DispenserStopHandlerFunc) Handle(params DispenserStopParams) DispenserStopResponder {
	return fn(params)
}

// DispenserStopHandler interface for that can handle valid dispenser stop params
type DispenserStopHandler interface {
	Handle(DispenserStopParams) DispenserStopResponder
}

// NewDispenserStop creates a new http.Handler for the dispenser stop operation
func NewDispenserStop(ctx *middleware.Context, handler DispenserStopHandler) *DispenserStop {
	return &DispenserStop{Context: ctx, Handler: handler}
}

/*
	DispenserStop swagger:route POST /stop-dispenser dispenserStop

DispenserStop dispenser stop API
*/
type DispenserStop struct {
	Context *middleware.Context
	Handler DispenserStopHandler
}

func (o *DispenserStop) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewDispenserStopParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// DispenserStopBody ArgDispenserStop
//
// swagger:model DispenserStopBody
type DispenserStopBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// stop program ID
	// Required: true
	StopProgramID *int64 `json:"stopProgramID"`
}

// Validate validates this dispenser stop body
func (o *DispenserStopBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStopProgramID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DispenserStopBody) validateHash(formats strfmt.Registry) error {

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
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

func (o *DispenserStopBody) validateStopProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stopProgramID", "body", o.StopProgramID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this dispenser stop body based on the context it is used
func (o *DispenserStopBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DispenserStopBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {
		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *DispenserStopBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DispenserStopBody) UnmarshalBinary(b []byte) error {
	var res DispenserStopBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
