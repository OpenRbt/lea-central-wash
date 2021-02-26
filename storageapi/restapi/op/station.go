// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	errors "github.com/go-openapi/errors"
	middleware "github.com/go-openapi/runtime/middleware"
	strfmt "github.com/go-openapi/strfmt"
	swag "github.com/go-openapi/swag"
	validate "github.com/go-openapi/validate"
)

// StationHandlerFunc turns a function with the right signature into a station handler
type StationHandlerFunc func(StationParams) StationResponder

// Handle executing the request and returning a response
func (fn StationHandlerFunc) Handle(params StationParams) StationResponder {
	return fn(params)
}

// StationHandler interface for that can handle valid station params
type StationHandler interface {
	Handle(StationParams) StationResponder
}

// NewStation creates a new http.Handler for the station operation
func NewStation(ctx *middleware.Context, handler StationHandler) *Station {
	return &Station{Context: ctx, Handler: handler}
}

/*Station swagger:route POST /station station

Station station API

*/
type Station struct {
	Context *middleware.Context
	Handler StationHandler
}

func (o *Station) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewStationParams()

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request

	o.Context.Respond(rw, r, route.Produces, route, res)

}

// StationBody station body
// swagger:model StationBody
type StationBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// Validate validates this station body
func (o *StationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationBody) UnmarshalBinary(b []byte) error {
	var res StationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
