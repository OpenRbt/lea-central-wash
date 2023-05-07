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

	"github.com/DiaElectronics/lea-central-wash/storageapi"
)

// GetStationConfigVarIntHandlerFunc turns a function with the right signature into a get station config var int handler
type GetStationConfigVarIntHandlerFunc func(GetStationConfigVarIntParams, *storageapi.Profile) GetStationConfigVarIntResponder

// Handle executing the request and returning a response
func (fn GetStationConfigVarIntHandlerFunc) Handle(params GetStationConfigVarIntParams, principal *storageapi.Profile) GetStationConfigVarIntResponder {
	return fn(params, principal)
}

// GetStationConfigVarIntHandler interface for that can handle valid get station config var int params
type GetStationConfigVarIntHandler interface {
	Handle(GetStationConfigVarIntParams, *storageapi.Profile) GetStationConfigVarIntResponder
}

// NewGetStationConfigVarInt creates a new http.Handler for the get station config var int operation
func NewGetStationConfigVarInt(ctx *middleware.Context, handler GetStationConfigVarIntHandler) *GetStationConfigVarInt {
	return &GetStationConfigVarInt{Context: ctx, Handler: handler}
}

/*
	GetStationConfigVarInt swagger:route POST /get-station-config-var-int getStationConfigVarInt

GetStationConfigVarInt get station config var int API
*/
type GetStationConfigVarInt struct {
	Context *middleware.Context
	Handler GetStationConfigVarIntHandler
}

func (o *GetStationConfigVarInt) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetStationConfigVarIntParams()
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

// GetStationConfigVarIntBody ArgGetStationConfigVar
//
// swagger:model GetStationConfigVarIntBody
type GetStationConfigVarIntBody struct {

	// name
	// Required: true
	Name *string `json:"name"`

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// Validate validates this get station config var int body
func (o *GetStationConfigVarIntBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateName(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetStationConfigVarIntBody) validateName(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"name", "body", o.Name); err != nil {
		return err
	}

	return nil
}

func (o *GetStationConfigVarIntBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get station config var int body based on context it is used
func (o *GetStationConfigVarIntBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationConfigVarIntBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationConfigVarIntBody) UnmarshalBinary(b []byte) error {
	var res GetStationConfigVarIntBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
