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

// StationStatDatesHandlerFunc turns a function with the right signature into a station stat dates handler
type StationStatDatesHandlerFunc func(StationStatDatesParams, *storageapi.Profile) StationStatDatesResponder

// Handle executing the request and returning a response
func (fn StationStatDatesHandlerFunc) Handle(params StationStatDatesParams, principal *storageapi.Profile) StationStatDatesResponder {
	return fn(params, principal)
}

// StationStatDatesHandler interface for that can handle valid station stat dates params
type StationStatDatesHandler interface {
	Handle(StationStatDatesParams, *storageapi.Profile) StationStatDatesResponder
}

// NewStationStatDates creates a new http.Handler for the station stat dates operation
func NewStationStatDates(ctx *middleware.Context, handler StationStatDatesHandler) *StationStatDates {
	return &StationStatDates{Context: ctx, Handler: handler}
}

/* StationStatDates swagger:route POST /station-stat-dates stationStatDates

StationStatDates station stat dates API

*/
type StationStatDates struct {
	Context *middleware.Context
	Handler StationStatDatesHandler
}

func (o *StationStatDates) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewStationStatDatesParams()
	uprinc, aCtx, err := o.Context.Authorize(r, route)
	if err != nil {
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}
	if aCtx != nil {
		r = aCtx
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

// StationStatDatesBody ArgStationStatDates
//
// swagger:model StationStatDatesBody
type StationStatDatesBody struct {

	// Unix time
	// Required: true
	EndDate *int64 `json:"endDate"`

	// Unix time
	// Required: true
	StartDate *int64 `json:"startDate"`

	// station ID
	StationID *int64 `json:"stationID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationStatDatesBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// Unix time
		// Required: true
		EndDate *int64 `json:"endDate"`

		// Unix time
		// Required: true
		StartDate *int64 `json:"startDate"`

		// station ID
		StationID *int64 `json:"stationID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.EndDate = props.EndDate
	o.StartDate = props.StartDate
	o.StationID = props.StationID
	return nil
}

// Validate validates this station stat dates body
func (o *StationStatDatesBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateEndDate(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStartDate(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationStatDatesBody) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"endDate", "body", o.EndDate); err != nil {
		return err
	}

	return nil
}

func (o *StationStatDatesBody) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startDate", "body", o.StartDate); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station stat dates body based on context it is used
func (o *StationStatDatesBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationStatDatesBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationStatDatesBody) UnmarshalBinary(b []byte) error {
	var res StationStatDatesBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
