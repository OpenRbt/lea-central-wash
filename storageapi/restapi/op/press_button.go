// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// PressButtonHandlerFunc turns a function with the right signature into a press button handler
type PressButtonHandlerFunc func(PressButtonParams) PressButtonResponder

// Handle executing the request and returning a response
func (fn PressButtonHandlerFunc) Handle(params PressButtonParams) PressButtonResponder {
	return fn(params)
}

// PressButtonHandler interface for that can handle valid press button params
type PressButtonHandler interface {
	Handle(PressButtonParams) PressButtonResponder
}

// NewPressButton creates a new http.Handler for the press button operation
func NewPressButton(ctx *middleware.Context, handler PressButtonHandler) *PressButton {
	return &PressButton{Context: ctx, Handler: handler}
}

/*
	PressButton swagger:route POST /press-button pressButton

PressButton press button API
*/
type PressButton struct {
	Context *middleware.Context
	Handler PressButtonHandler
}

func (o *PressButton) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewPressButtonParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// PressButtonBody ArgPressButton
//
// swagger:model PressButtonBody
type PressButtonBody struct {

	// button ID
	// Required: true
	ButtonID *int64 `json:"buttonID"`

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *PressButtonBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// button ID
		// Required: true
		ButtonID *int64 `json:"buttonID"`

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ButtonID = props.ButtonID
	o.Hash = props.Hash
	return nil
}

// Validate validates this press button body
func (o *PressButtonBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateButtonID(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PressButtonBody) validateButtonID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"buttonID", "body", o.ButtonID); err != nil {
		return err
	}

	return nil
}

func (o *PressButtonBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this press button body based on the context it is used
func (o *PressButtonBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PressButtonBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *PressButtonBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *PressButtonBody) UnmarshalBinary(b []byte) error {
	var res PressButtonBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
