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
)

// StationReportDatesHandlerFunc turns a function with the right signature into a station report dates handler
type StationReportDatesHandlerFunc func(StationReportDatesParams) StationReportDatesResponder

// Handle executing the request and returning a response
func (fn StationReportDatesHandlerFunc) Handle(params StationReportDatesParams) StationReportDatesResponder {
	return fn(params)
}

// StationReportDatesHandler interface for that can handle valid station report dates params
type StationReportDatesHandler interface {
	Handle(StationReportDatesParams) StationReportDatesResponder
}

// NewStationReportDates creates a new http.Handler for the station report dates operation
func NewStationReportDates(ctx *middleware.Context, handler StationReportDatesHandler) *StationReportDates {
	return &StationReportDates{Context: ctx, Handler: handler}
}

/*
	StationReportDates swagger:route POST /station-report-dates stationReportDates

StationReportDates station report dates API
*/
type StationReportDates struct {
	Context *middleware.Context
	Handler StationReportDatesHandler
}

func (o *StationReportDates) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewStationReportDatesParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// StationReportDatesBody ArgStationReportDates
//
// swagger:model StationReportDatesBody
type StationReportDatesBody struct {

	// Unix time
	// Required: true
	EndDate *int64 `json:"endDate"`

	// id
	// Required: true
	ID *int64 `json:"id"`

	// Unix time
	// Required: true
	StartDate *int64 `json:"startDate"`
}

// Validate validates this station report dates body
func (o *StationReportDatesBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateEndDate(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateID(formats); err != nil {
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

func (o *StationReportDatesBody) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"endDate", "body", o.EndDate); err != nil {
		return err
	}

	return nil
}

func (o *StationReportDatesBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

func (o *StationReportDatesBody) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startDate", "body", o.StartDate); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station report dates body based on context it is used
func (o *StationReportDatesBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationReportDatesBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationReportDatesBody) UnmarshalBinary(b []byte) error {
	var res StationReportDatesBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
