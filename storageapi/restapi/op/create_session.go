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
)

// CreateSessionHandlerFunc turns a function with the right signature into a create session handler
type CreateSessionHandlerFunc func(CreateSessionParams) CreateSessionResponder

// Handle executing the request and returning a response
func (fn CreateSessionHandlerFunc) Handle(params CreateSessionParams) CreateSessionResponder {
	return fn(params)
}

// CreateSessionHandler interface for that can handle valid create session params
type CreateSessionHandler interface {
	Handle(CreateSessionParams) CreateSessionResponder
}

// NewCreateSession creates a new http.Handler for the create session operation
func NewCreateSession(ctx *middleware.Context, handler CreateSessionHandler) *CreateSession {
	return &CreateSession{Context: ctx, Handler: handler}
}

/*
	CreateSession swagger:route POST /create-session createSession

CreateSession create session API
*/
type CreateSession struct {
	Context *middleware.Context
	Handler CreateSessionHandler
}

func (o *CreateSession) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewCreateSessionParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// CreateSessionBody CreateSession
//
// swagger:model CreateSessionBody
type CreateSessionBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CreateSessionBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	return nil
}

// Validate validates this create session body
func (o *CreateSessionBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CreateSessionBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this create session body based on context it is used
func (o *CreateSessionBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *CreateSessionBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CreateSessionBody) UnmarshalBinary(b []byte) error {
	var res CreateSessionBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
