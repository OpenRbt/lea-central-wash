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

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// GetConfigVarIntHandlerFunc turns a function with the right signature into a get config var int handler
type GetConfigVarIntHandlerFunc func(GetConfigVarIntParams, *storageapi.Profile) GetConfigVarIntResponder

// Handle executing the request and returning a response
func (fn GetConfigVarIntHandlerFunc) Handle(params GetConfigVarIntParams, principal *storageapi.Profile) GetConfigVarIntResponder {
	return fn(params, principal)
}

// GetConfigVarIntHandler interface for that can handle valid get config var int params
type GetConfigVarIntHandler interface {
	Handle(GetConfigVarIntParams, *storageapi.Profile) GetConfigVarIntResponder
}

// NewGetConfigVarInt creates a new http.Handler for the get config var int operation
func NewGetConfigVarInt(ctx *middleware.Context, handler GetConfigVarIntHandler) *GetConfigVarInt {
	return &GetConfigVarInt{Context: ctx, Handler: handler}
}

/*
	GetConfigVarInt swagger:route POST /get-config-var-int getConfigVarInt

GetConfigVarInt get config var int API
*/
type GetConfigVarInt struct {
	Context *middleware.Context
	Handler GetConfigVarIntHandler
}

func (o *GetConfigVarInt) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetConfigVarIntParams()
	uprinc, aCtx, err := o.Context.Authorize(r, route)
	if err != nil {
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}
	if aCtx != nil {
		*r = *aCtx
	}
	var principal *storageapi.Profile
	if uprinc != nil {
		principal = uprinc.(*storageapi.Profile) // this is really a storageapi.Profile, I promise
	}

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params, principal) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// GetConfigVarIntBody ArgGetConfigVar
//
// swagger:model GetConfigVarIntBody
type GetConfigVarIntBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetConfigVarIntBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// name
		Name string `json:"name,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Name = props.Name
	return nil
}

// Validate validates this get config var int body
func (o *GetConfigVarIntBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var int body based on context it is used
func (o *GetConfigVarIntBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarIntBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarIntBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarIntBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
