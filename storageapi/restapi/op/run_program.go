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

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// RunProgramHandlerFunc turns a function with the right signature into a run program handler
type RunProgramHandlerFunc func(RunProgramParams) RunProgramResponder

// Handle executing the request and returning a response
func (fn RunProgramHandlerFunc) Handle(params RunProgramParams) RunProgramResponder {
	return fn(params)
}

// RunProgramHandler interface for that can handle valid run program params
type RunProgramHandler interface {
	Handle(RunProgramParams) RunProgramResponder
}

// NewRunProgram creates a new http.Handler for the run program operation
func NewRunProgram(ctx *middleware.Context, handler RunProgramHandler) *RunProgram {
	return &RunProgram{Context: ctx, Handler: handler}
}

/*
	RunProgram swagger:route POST /run-program runProgram

RunProgram run program API
*/
type RunProgram struct {
	Context *middleware.Context
	Handler RunProgramHandler
}

func (o *RunProgram) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewRunProgramParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// RunProgramBody ArgRunProgram
//
// swagger:model RunProgramBody
type RunProgramBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// preflight
	// Required: true
	Preflight *bool `json:"preflight"`

	// program ID
	// Required: true
	ProgramID *int64 `json:"programID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *RunProgramBody) UnmarshalJSON(data []byte) error {
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
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Preflight = props.Preflight
	o.ProgramID = props.ProgramID
	return nil
}

// Validate validates this run program body
func (o *RunProgramBody) Validate(formats strfmt.Registry) error {
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

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *RunProgramBody) validateHash(formats strfmt.Registry) error {

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

func (o *RunProgramBody) validatePreflight(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"preflight", "body", o.Preflight); err != nil {
		return err
	}

	return nil
}

func (o *RunProgramBody) validateProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"programID", "body", o.ProgramID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this run program body based on the context it is used
func (o *RunProgramBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *RunProgramBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *RunProgramBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *RunProgramBody) UnmarshalBinary(b []byte) error {
	var res RunProgramBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
