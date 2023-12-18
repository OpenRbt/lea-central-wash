// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"

	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// AddServiceAmountHandlerFunc turns a function with the right signature into a add service amount handler
type AddServiceAmountHandlerFunc func(AddServiceAmountParams) AddServiceAmountResponder

// Handle executing the request and returning a response
func (fn AddServiceAmountHandlerFunc) Handle(params AddServiceAmountParams) AddServiceAmountResponder {
	return fn(params)
}

// AddServiceAmountHandler interface for that can handle valid add service amount params
type AddServiceAmountHandler interface {
	Handle(AddServiceAmountParams) AddServiceAmountResponder
}

// NewAddServiceAmount creates a new http.Handler for the add service amount operation
func NewAddServiceAmount(ctx *middleware.Context, handler AddServiceAmountHandler) *AddServiceAmount {
	return &AddServiceAmount{Context: ctx, Handler: handler}
}

/*
	AddServiceAmount swagger:route POST /add-service-amount addServiceAmount

AddServiceAmount add service amount API
*/
type AddServiceAmount struct {
	Context *middleware.Context
	Handler AddServiceAmountHandler
}

func (o *AddServiceAmount) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewAddServiceAmountParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// AddServiceAmountBody ArgAddServiceAmount
//
// swagger:model AddServiceAmountBody
type AddServiceAmountBody struct {

	// amount
	Amount int64 `json:"amount,omitempty"`

	// hash
	Hash string `json:"hash,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AddServiceAmountBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// amount
		Amount int64 `json:"amount,omitempty"`

		// hash
		Hash string `json:"hash,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Amount = props.Amount
	o.Hash = props.Hash
	return nil
}

// Validate validates this add service amount body
func (o *AddServiceAmountBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this add service amount body based on context it is used
func (o *AddServiceAmountBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AddServiceAmountBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AddServiceAmountBody) UnmarshalBinary(b []byte) error {
	var res AddServiceAmountBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
