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

// GetConfigVarBoolHandlerFunc turns a function with the right signature into a get config var bool handler
type GetConfigVarBoolHandlerFunc func(GetConfigVarBoolParams) GetConfigVarBoolResponder

// Handle executing the request and returning a response
func (fn GetConfigVarBoolHandlerFunc) Handle(params GetConfigVarBoolParams) GetConfigVarBoolResponder {
	return fn(params)
}

// GetConfigVarBoolHandler interface for that can handle valid get config var bool params
type GetConfigVarBoolHandler interface {
	Handle(GetConfigVarBoolParams) GetConfigVarBoolResponder
}

// NewGetConfigVarBool creates a new http.Handler for the get config var bool operation
func NewGetConfigVarBool(ctx *middleware.Context, handler GetConfigVarBoolHandler) *GetConfigVarBool {
	return &GetConfigVarBool{Context: ctx, Handler: handler}
}

/* GetConfigVarBool swagger:route POST /get-config-var-bool getConfigVarBool

GetConfigVarBool get config var bool API

*/
type GetConfigVarBool struct {
	Context *middleware.Context
	Handler GetConfigVarBoolHandler
}

func (o *GetConfigVarBool) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetConfigVarBoolParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// GetConfigVarBoolBody ArgGetConfigVar
//
// swagger:model GetConfigVarBoolBody
type GetConfigVarBoolBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetConfigVarBoolBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this get config var bool body
func (o *GetConfigVarBoolBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var bool body based on context it is used
func (o *GetConfigVarBoolBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarBoolBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarBoolBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarBoolBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
