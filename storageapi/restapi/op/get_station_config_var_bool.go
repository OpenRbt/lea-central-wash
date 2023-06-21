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

	"github.com/DiaElectronics/lea-central-wash/storageapi"
)

// GetStationConfigVarBoolHandlerFunc turns a function with the right signature into a get station config var bool handler
type GetStationConfigVarBoolHandlerFunc func(GetStationConfigVarBoolParams, *storageapi.Profile) GetStationConfigVarBoolResponder

// Handle executing the request and returning a response
func (fn GetStationConfigVarBoolHandlerFunc) Handle(params GetStationConfigVarBoolParams, principal *storageapi.Profile) GetStationConfigVarBoolResponder {
	return fn(params, principal)
}

// GetStationConfigVarBoolHandler interface for that can handle valid get station config var bool params
type GetStationConfigVarBoolHandler interface {
	Handle(GetStationConfigVarBoolParams, *storageapi.Profile) GetStationConfigVarBoolResponder
}

// NewGetStationConfigVarBool creates a new http.Handler for the get station config var bool operation
func NewGetStationConfigVarBool(ctx *middleware.Context, handler GetStationConfigVarBoolHandler) *GetStationConfigVarBool {
	return &GetStationConfigVarBool{Context: ctx, Handler: handler}
}

/* GetStationConfigVarBool swagger:route POST /get-station-config-var-bool getStationConfigVarBool

GetStationConfigVarBool get station config var bool API

*/
type GetStationConfigVarBool struct {
	Context *middleware.Context
	Handler GetStationConfigVarBoolHandler
}

func (o *GetStationConfigVarBool) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewGetStationConfigVarBoolParams()
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

// GetStationConfigVarBoolBody ArgGetStationConfigVar
//
// swagger:model GetStationConfigVarBoolBody
type GetStationConfigVarBoolBody struct {

	// name
	// Required: true
	Name *string `json:"name"`

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationConfigVarBoolBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// name
		// Required: true
		Name *string `json:"name"`

		// station ID
		// Required: true
		StationID *int64 `json:"stationID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Name = props.Name
	o.StationID = props.StationID
	return nil
}

// Validate validates this get station config var bool body
func (o *GetStationConfigVarBoolBody) Validate(formats strfmt.Registry) error {
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

func (o *GetStationConfigVarBoolBody) validateName(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"name", "body", o.Name); err != nil {
		return err
	}

	return nil
}

func (o *GetStationConfigVarBoolBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get station config var bool body based on context it is used
func (o *GetStationConfigVarBoolBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationConfigVarBoolBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationConfigVarBoolBody) UnmarshalBinary(b []byte) error {
	var res GetStationConfigVarBoolBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
