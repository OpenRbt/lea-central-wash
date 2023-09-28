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

// GetLevelHandlerFunc turns a function with the right signature into a get level handler
type GetLevelHandlerFunc func(GetLevelParams) GetLevelResponder

// Handle executing the request and returning a response
func (fn GetLevelHandlerFunc) Handle(params GetLevelParams) GetLevelResponder {
	return fn(params)
}

// GetLevelHandler interface for that can handle valid get level params
type GetLevelHandler interface {
	Handle(GetLevelParams) GetLevelResponder
}

// NewGetLevel creates a new http.Handler for the get level operation
func NewGetLevel(ctx *middleware.Context, handler GetLevelHandler) *GetLevel {
	return &GetLevel{Context: ctx, Handler: handler}
}

/*
	GetLevel swagger:route POST /getLevel getLevel

GetLevel get level API
*/
type GetLevel struct {
	Context *middleware.Context
	Handler GetLevelHandler
}

func (o *GetLevel) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetLevelParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// GetLevelBody ArgGetLevel
//
// swagger:model GetLevelBody
type GetLevelBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// Validate validates this get level body
func (o *GetLevelBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this get level body based on the context it is used
func (o *GetLevelBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *GetLevelBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetLevelBody) UnmarshalBinary(b []byte) error {
	var res GetLevelBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// GetLevelOKBody ResponseGetLevel
//
// swagger:model GetLevelOKBody
type GetLevelOKBody struct {

	// level
	// Required: true
	Level *int64 `json:"level"`
}

// Validate validates this get level o k body
func (o *GetLevelOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateLevel(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelOKBody) validateLevel(formats strfmt.Registry) error {

	if err := validate.Required("getLevelOK"+"."+"level", "body", o.Level); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get level o k body based on context it is used
func (o *GetLevelOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetLevelOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetLevelOKBody) UnmarshalBinary(b []byte) error {
	var res GetLevelOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
