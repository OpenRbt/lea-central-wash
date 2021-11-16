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

// SaveHandlerFunc turns a function with the right signature into a save handler
type SaveHandlerFunc func(SaveParams) SaveResponder

// Handle executing the request and returning a response
func (fn SaveHandlerFunc) Handle(params SaveParams) SaveResponder {
	return fn(params)
}

// SaveHandler interface for that can handle valid save params
type SaveHandler interface {
	Handle(SaveParams) SaveResponder
}

// NewSave creates a new http.Handler for the save operation
func NewSave(ctx *middleware.Context, handler SaveHandler) *Save {
	return &Save{Context: ctx, Handler: handler}
}

/* Save swagger:route POST /save save

Save save API

*/
type Save struct {
	Context *middleware.Context
	Handler SaveHandler
}

func (o *Save) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSaveParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// SaveBody save body
//
// swagger:model SaveBody
type SaveBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// key pair
	// Required: true
	KeyPair *model.KeyPair `json:"keyPair"`
}

// Validate validates this save body
func (o *SaveBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateKeyPair(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SaveBody) validateHash(formats strfmt.Registry) error {

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

func (o *SaveBody) validateKeyPair(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"keyPair", "body", o.KeyPair); err != nil {
		return err
	}

	if o.KeyPair != nil {
		if err := o.KeyPair.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "keyPair")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this save body based on the context it is used
func (o *SaveBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateKeyPair(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SaveBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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

func (o *SaveBody) contextValidateKeyPair(ctx context.Context, formats strfmt.Registry) error {

	if o.KeyPair != nil {
		if err := o.KeyPair.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "keyPair")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *SaveBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SaveBody) UnmarshalBinary(b []byte) error {
	var res SaveBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
