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

// GetStationWashConfigVarIntHandlerFunc turns a function with the right signature into a get station wash config var int handler
type GetStationWashConfigVarIntHandlerFunc func(GetStationWashConfigVarIntParams) GetStationWashConfigVarIntResponder

// Handle executing the request and returning a response
func (fn GetStationWashConfigVarIntHandlerFunc) Handle(params GetStationWashConfigVarIntParams) GetStationWashConfigVarIntResponder {
	return fn(params)
}

// GetStationWashConfigVarIntHandler interface for that can handle valid get station wash config var int params
type GetStationWashConfigVarIntHandler interface {
	Handle(GetStationWashConfigVarIntParams) GetStationWashConfigVarIntResponder
}

// NewGetStationWashConfigVarInt creates a new http.Handler for the get station wash config var int operation
func NewGetStationWashConfigVarInt(ctx *middleware.Context, handler GetStationWashConfigVarIntHandler) *GetStationWashConfigVarInt {
	return &GetStationWashConfigVarInt{Context: ctx, Handler: handler}
}

/*
	GetStationWashConfigVarInt swagger:route POST /get-wash-config-var-int getStationWashConfigVarInt

GetStationWashConfigVarInt get station wash config var int API
*/
type GetStationWashConfigVarInt struct {
	Context *middleware.Context
	Handler GetStationWashConfigVarIntHandler
}

func (o *GetStationWashConfigVarInt) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetStationWashConfigVarIntParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// GetStationWashConfigVarIntBody ArgGetStationConfigVar
//
// swagger:model GetStationWashConfigVarIntBody
type GetStationWashConfigVarIntBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`

	// name
	// Required: true
	Name *string `json:"name"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationWashConfigVarIntBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`

		// name
		// Required: true
		Name *string `json:"name"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Name = props.Name
	return nil
}

// Validate validates this get station wash config var int body
func (o *GetStationWashConfigVarIntBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateName(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetStationWashConfigVarIntBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

func (o *GetStationWashConfigVarIntBody) validateName(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"name", "body", o.Name); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get station wash config var int body based on context it is used
func (o *GetStationWashConfigVarIntBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationWashConfigVarIntBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationWashConfigVarIntBody) UnmarshalBinary(b []byte) error {
	var res GetStationWashConfigVarIntBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
