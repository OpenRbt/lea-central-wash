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

// StationReportCurrentMoneyHandlerFunc turns a function with the right signature into a station report current money handler
type StationReportCurrentMoneyHandlerFunc func(StationReportCurrentMoneyParams) StationReportCurrentMoneyResponder

// Handle executing the request and returning a response
func (fn StationReportCurrentMoneyHandlerFunc) Handle(params StationReportCurrentMoneyParams) StationReportCurrentMoneyResponder {
	return fn(params)
}

// StationReportCurrentMoneyHandler interface for that can handle valid station report current money params
type StationReportCurrentMoneyHandler interface {
	Handle(StationReportCurrentMoneyParams) StationReportCurrentMoneyResponder
}

// NewStationReportCurrentMoney creates a new http.Handler for the station report current money operation
func NewStationReportCurrentMoney(ctx *middleware.Context, handler StationReportCurrentMoneyHandler) *StationReportCurrentMoney {
	return &StationReportCurrentMoney{Context: ctx, Handler: handler}
}

/* StationReportCurrentMoney swagger:route POST /station-report-current-money stationReportCurrentMoney

StationReportCurrentMoney station report current money API

*/
type StationReportCurrentMoney struct {
	Context *middleware.Context
	Handler StationReportCurrentMoneyHandler
}

func (o *StationReportCurrentMoney) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewStationReportCurrentMoneyParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// StationReportCurrentMoneyBody station report current money body
//
// swagger:model StationReportCurrentMoneyBody
type StationReportCurrentMoneyBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// Validate validates this station report current money body
func (o *StationReportCurrentMoneyBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationReportCurrentMoneyBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station report current money body based on context it is used
func (o *StationReportCurrentMoneyBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationReportCurrentMoneyBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationReportCurrentMoneyBody) UnmarshalBinary(b []byte) error {
	var res StationReportCurrentMoneyBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
