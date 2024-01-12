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

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// ResetStationStatHandlerFunc turns a function with the right signature into a reset station stat handler
type ResetStationStatHandlerFunc func(ResetStationStatParams, *storageapi.Profile) ResetStationStatResponder

// Handle executing the request and returning a response
func (fn ResetStationStatHandlerFunc) Handle(params ResetStationStatParams, principal *storageapi.Profile) ResetStationStatResponder {
	return fn(params, principal)
}

// ResetStationStatHandler interface for that can handle valid reset station stat params
type ResetStationStatHandler interface {
	Handle(ResetStationStatParams, *storageapi.Profile) ResetStationStatResponder
}

// NewResetStationStat creates a new http.Handler for the reset station stat operation
func NewResetStationStat(ctx *middleware.Context, handler ResetStationStatHandler) *ResetStationStat {
	return &ResetStationStat{Context: ctx, Handler: handler}
}

/*
	ResetStationStat swagger:route POST /reset-station-stat resetStationStat

ResetStationStat reset station stat API
*/
type ResetStationStat struct {
	Context *middleware.Context
	Handler ResetStationStatHandler
}

func (o *ResetStationStat) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewResetStationStatParams()
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

// ResetStationStatBody ArgResetStationStat
//
// swagger:model ResetStationStatBody
type ResetStationStatBody struct {

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *ResetStationStatBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// station ID
		// Required: true
		StationID *int64 `json:"stationID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.StationID = props.StationID
	return nil
}

// Validate validates this reset station stat body
func (o *ResetStationStatBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *ResetStationStatBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this reset station stat body based on context it is used
func (o *ResetStationStatBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *ResetStationStatBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *ResetStationStatBody) UnmarshalBinary(b []byte) error {
	var res ResetStationStatBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
