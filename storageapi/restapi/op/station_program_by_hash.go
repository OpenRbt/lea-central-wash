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

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// StationProgramByHashHandlerFunc turns a function with the right signature into a station program by hash handler
type StationProgramByHashHandlerFunc func(StationProgramByHashParams) StationProgramByHashResponder

// Handle executing the request and returning a response
func (fn StationProgramByHashHandlerFunc) Handle(params StationProgramByHashParams) StationProgramByHashResponder {
	return fn(params)
}

// StationProgramByHashHandler interface for that can handle valid station program by hash params
type StationProgramByHashHandler interface {
	Handle(StationProgramByHashParams) StationProgramByHashResponder
}

// NewStationProgramByHash creates a new http.Handler for the station program by hash operation
func NewStationProgramByHash(ctx *middleware.Context, handler StationProgramByHashHandler) *StationProgramByHash {
	return &StationProgramByHash{Context: ctx, Handler: handler}
}

/*
	StationProgramByHash swagger:route POST /station-program-by-hash stationProgramByHash

StationProgramByHash station program by hash API
*/
type StationProgramByHash struct {
	Context *middleware.Context
	Handler StationProgramByHashHandler
}

func (o *StationProgramByHash) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewStationProgramByHashParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// StationProgramByHashBody ArgStationProgramByHash
//
// swagger:model StationProgramByHashBody
type StationProgramByHashBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// Validate validates this station program by hash body
func (o *StationProgramByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationProgramByHashBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this station program by hash body based on the context it is used
func (o *StationProgramByHashBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationProgramByHashBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *StationProgramByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationProgramByHashBody) UnmarshalBinary(b []byte) error {
	var res StationProgramByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
