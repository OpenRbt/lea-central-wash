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

// MeasureVolumeMillilitersHandlerFunc turns a function with the right signature into a measure volume milliliters handler
type MeasureVolumeMillilitersHandlerFunc func(MeasureVolumeMillilitersParams) MeasureVolumeMillilitersResponder

// Handle executing the request and returning a response
func (fn MeasureVolumeMillilitersHandlerFunc) Handle(params MeasureVolumeMillilitersParams) MeasureVolumeMillilitersResponder {
	return fn(params)
}

// MeasureVolumeMillilitersHandler interface for that can handle valid measure volume milliliters params
type MeasureVolumeMillilitersHandler interface {
	Handle(MeasureVolumeMillilitersParams) MeasureVolumeMillilitersResponder
}

// NewMeasureVolumeMilliliters creates a new http.Handler for the measure volume milliliters operation
func NewMeasureVolumeMilliliters(ctx *middleware.Context, handler MeasureVolumeMillilitersHandler) *MeasureVolumeMilliliters {
	return &MeasureVolumeMilliliters{Context: ctx, Handler: handler}
}

/*
	MeasureVolumeMilliliters swagger:route POST /run-dispenser measureVolumeMilliliters

MeasureVolumeMilliliters measure volume milliliters API
*/
type MeasureVolumeMilliliters struct {
	Context *middleware.Context
	Handler MeasureVolumeMillilitersHandler
}

func (o *MeasureVolumeMilliliters) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewMeasureVolumeMillilitersParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// MeasureVolumeMillilitersBody ArgMeasureVolumeMilliliters
//
// swagger:model MeasureVolumeMillilitersBody
type MeasureVolumeMillilitersBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// start program ID
	// Required: true
	StartProgramID *int64 `json:"startProgramID"`

	// stop program ID
	// Required: true
	StopProgramID *int64 `json:"stopProgramID"`

	// volume
	// Required: true
	Volume *int64 `json:"volume"`
}

// Validate validates this measure volume milliliters body
func (o *MeasureVolumeMillilitersBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStartProgramID(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStopProgramID(formats); err != nil {
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

func (o *MeasureVolumeMillilitersBody) validateHash(formats strfmt.Registry) error {

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

func (o *MeasureVolumeMillilitersBody) validateStartProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startProgramID", "body", o.StartProgramID); err != nil {
		return err
	}

	return nil
}

func (o *MeasureVolumeMillilitersBody) validateStopProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stopProgramID", "body", o.StopProgramID); err != nil {
		return err
	}

	return nil
}

func (o *MeasureVolumeMillilitersBody) validateVolume(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"volume", "body", o.Volume); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this measure volume milliliters body based on the context it is used
func (o *MeasureVolumeMillilitersBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *MeasureVolumeMillilitersBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *MeasureVolumeMillilitersBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *MeasureVolumeMillilitersBody) UnmarshalBinary(b []byte) error {
	var res MeasureVolumeMillilitersBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
