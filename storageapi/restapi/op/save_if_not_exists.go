// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	errors "github.com/go-openapi/errors"
	middleware "github.com/go-openapi/runtime/middleware"
	strfmt "github.com/go-openapi/strfmt"
	swag "github.com/go-openapi/swag"
	validate "github.com/go-openapi/validate"

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// SaveIfNotExistsHandlerFunc turns a function with the right signature into a save if not exists handler
type SaveIfNotExistsHandlerFunc func(SaveIfNotExistsParams) SaveIfNotExistsResponder

// Handle executing the request and returning a response
func (fn SaveIfNotExistsHandlerFunc) Handle(params SaveIfNotExistsParams) SaveIfNotExistsResponder {
	return fn(params)
}

// SaveIfNotExistsHandler interface for that can handle valid save if not exists params
type SaveIfNotExistsHandler interface {
	Handle(SaveIfNotExistsParams) SaveIfNotExistsResponder
}

// NewSaveIfNotExists creates a new http.Handler for the save if not exists operation
func NewSaveIfNotExists(ctx *middleware.Context, handler SaveIfNotExistsHandler) *SaveIfNotExists {
	return &SaveIfNotExists{Context: ctx, Handler: handler}
}

/*SaveIfNotExists swagger:route POST /save-if-not-exists saveIfNotExists

SaveIfNotExists save if not exists API

*/
type SaveIfNotExists struct {
	Context *middleware.Context
	Handler SaveIfNotExistsHandler
}

func (o *SaveIfNotExists) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewSaveIfNotExistsParams()

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request

	o.Context.Respond(rw, r, route.Produces, route, res)

}

// SaveIfNotExistsBody save if not exists body
// swagger:model SaveIfNotExistsBody
type SaveIfNotExistsBody struct {

	// hash
	// Required: true
	Hash model.Hash `json:"hash"`

	// key pair
	// Required: true
	KeyPair *model.KeyPair `json:"keyPair"`
}

// Validate validates this save if not exists body
func (o *SaveIfNotExistsBody) Validate(formats strfmt.Registry) error {
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

func (o *SaveIfNotExistsBody) validateHash(formats strfmt.Registry) error {

	if err := o.Hash.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("args" + "." + "hash")
		}
		return err
	}

	return nil
}

func (o *SaveIfNotExistsBody) validateKeyPair(formats strfmt.Registry) error {

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

// MarshalBinary interface implementation
func (o *SaveIfNotExistsBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SaveIfNotExistsBody) UnmarshalBinary(b []byte) error {
	var res SaveIfNotExistsBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
