// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"context"
	"net/http"

	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// GetConfigVarStringHandlerFunc turns a function with the right signature into a get config var string handler
type GetConfigVarStringHandlerFunc func(GetConfigVarStringParams, *storageapi.Profile) GetConfigVarStringResponder

// Handle executing the request and returning a response
func (fn GetConfigVarStringHandlerFunc) Handle(params GetConfigVarStringParams, principal *storageapi.Profile) GetConfigVarStringResponder {
	return fn(params, principal)
}

// GetConfigVarStringHandler interface for that can handle valid get config var string params
type GetConfigVarStringHandler interface {
	Handle(GetConfigVarStringParams, *storageapi.Profile) GetConfigVarStringResponder
}

// NewGetConfigVarString creates a new http.Handler for the get config var string operation
func NewGetConfigVarString(ctx *middleware.Context, handler GetConfigVarStringHandler) *GetConfigVarString {
	return &GetConfigVarString{Context: ctx, Handler: handler}
}

/*
	GetConfigVarString swagger:route POST /get-config-var-string getConfigVarString

GetConfigVarString get config var string API
*/
type GetConfigVarString struct {
	Context *middleware.Context
	Handler GetConfigVarStringHandler
}

func (o *GetConfigVarString) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetConfigVarStringParams()
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

// GetConfigVarStringBody ArgGetConfigVar
//
// swagger:model GetConfigVarStringBody
type GetConfigVarStringBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// Validate validates this get config var string body
func (o *GetConfigVarStringBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var string body based on context it is used
func (o *GetConfigVarStringBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarStringBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarStringBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarStringBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
