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

// VolumeDispenserHandlerFunc turns a function with the right signature into a volume dispenser handler
type VolumeDispenserHandlerFunc func(VolumeDispenserParams) VolumeDispenserResponder

// Handle executing the request and returning a response
func (fn VolumeDispenserHandlerFunc) Handle(params VolumeDispenserParams) VolumeDispenserResponder {
	return fn(params)
}

// VolumeDispenserHandler interface for that can handle valid volume dispenser params
type VolumeDispenserHandler interface {
	Handle(VolumeDispenserParams) VolumeDispenserResponder
}

// NewVolumeDispenser creates a new http.Handler for the volume dispenser operation
func NewVolumeDispenser(ctx *middleware.Context, handler VolumeDispenserHandler) *VolumeDispenser {
	return &VolumeDispenser{Context: ctx, Handler: handler}
}

/*
	VolumeDispenser swagger:route POST /volume-dispenser volumeDispenser

VolumeDispenser volume dispenser API
*/
type VolumeDispenser struct {
	Context *middleware.Context
	Handler VolumeDispenserHandler
}

func (o *VolumeDispenser) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewVolumeDispenserParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// VolumeDispenserBody VolumeDispenser
//
// swagger:model VolumeDispenserBody
type VolumeDispenserBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// Validate validates this volume dispenser body
func (o *VolumeDispenserBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *VolumeDispenserBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this volume dispenser body based on the context it is used
func (o *VolumeDispenserBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *VolumeDispenserBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *VolumeDispenserBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *VolumeDispenserBody) UnmarshalBinary(b []byte) error {
	var res VolumeDispenserBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// VolumeDispenserOKBody ResponseVolumeDispenser
//
// swagger:model VolumeDispenserOKBody
type VolumeDispenserOKBody struct {

	// status
	// Required: true
	Status *string `json:"status"`

	// volume
	// Required: true
	Volume *int64 `json:"volume"`
}

// Validate validates this volume dispenser o k body
func (o *VolumeDispenserOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStatus(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateVolume(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *VolumeDispenserOKBody) validateStatus(formats strfmt.Registry) error {

	if err := validate.Required("volumeDispenserOK"+"."+"status", "body", o.Status); err != nil {
		return err
	}

	return nil
}

func (o *VolumeDispenserOKBody) validateVolume(formats strfmt.Registry) error {

	if err := validate.Required("volumeDispenserOK"+"."+"volume", "body", o.Volume); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this volume dispenser o k body based on context it is used
func (o *VolumeDispenserOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *VolumeDispenserOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *VolumeDispenserOKBody) UnmarshalBinary(b []byte) error {
	var res VolumeDispenserOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
