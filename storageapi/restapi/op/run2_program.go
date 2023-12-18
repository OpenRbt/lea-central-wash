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

// Run2ProgramHandlerFunc turns a function with the right signature into a run2 program handler
type Run2ProgramHandlerFunc func(Run2ProgramParams) Run2ProgramResponder

// Handle executing the request and returning a response
func (fn Run2ProgramHandlerFunc) Handle(params Run2ProgramParams) Run2ProgramResponder {
	return fn(params)
}

// Run2ProgramHandler interface for that can handle valid run2 program params
type Run2ProgramHandler interface {
	Handle(Run2ProgramParams) Run2ProgramResponder
}

// NewRun2Program creates a new http.Handler for the run2 program operation
func NewRun2Program(ctx *middleware.Context, handler Run2ProgramHandler) *Run2Program {
	return &Run2Program{Context: ctx, Handler: handler}
}

/*
	Run2Program swagger:route POST /run-2program run2Program

Run2Program run2 program API
*/
type Run2Program struct {
	Context *middleware.Context
	Handler Run2ProgramHandler
}

func (o *Run2Program) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewRun2ProgramParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// Run2ProgramBody ArgRun2Program
//
// swagger:model Run2ProgramBody
type Run2ProgramBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// preflight
	// Required: true
	Preflight *bool `json:"preflight"`

	// program ID
	// Required: true
	ProgramID *int64 `json:"programID"`

	// program ID 2
	// Required: true
	ProgramID2 *int64 `json:"programID2"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *Run2ProgramBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// preflight
		// Required: true
		Preflight *bool `json:"preflight"`

		// program ID
		// Required: true
		ProgramID *int64 `json:"programID"`

		// program ID 2
		// Required: true
		ProgramID2 *int64 `json:"programID2"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Preflight = props.Preflight
	o.ProgramID = props.ProgramID
	o.ProgramID2 = props.ProgramID2
	return nil
}

// Validate validates this run2 program body
func (o *Run2ProgramBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validatePreflight(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateProgramID(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateProgramID2(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *Run2ProgramBody) validateHash(formats strfmt.Registry) error {

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

func (o *Run2ProgramBody) validatePreflight(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"preflight", "body", o.Preflight); err != nil {
		return err
	}

	return nil
}

func (o *Run2ProgramBody) validateProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"programID", "body", o.ProgramID); err != nil {
		return err
	}

	return nil
}

func (o *Run2ProgramBody) validateProgramID2(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"programID2", "body", o.ProgramID2); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this run2 program body based on the context it is used
func (o *Run2ProgramBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *Run2ProgramBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *Run2ProgramBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *Run2ProgramBody) UnmarshalBinary(b []byte) error {
	var res Run2ProgramBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
