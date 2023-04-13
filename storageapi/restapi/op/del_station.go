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
)

// DelStationHandlerFunc turns a function with the right signature into a del station handler
type DelStationHandlerFunc func(DelStationParams) DelStationResponder

// Handle executing the request and returning a response
func (fn DelStationHandlerFunc) Handle(params DelStationParams) DelStationResponder {
	return fn(params)
}

// DelStationHandler interface for that can handle valid del station params
type DelStationHandler interface {
	Handle(DelStationParams) DelStationResponder
}

// NewDelStation creates a new http.Handler for the del station operation
func NewDelStation(ctx *middleware.Context, handler DelStationHandler) *DelStation {
	return &DelStation{Context: ctx, Handler: handler}
}

/*
	DelStation swagger:route POST /del-station delStation

DelStation del station API
*/
type DelStation struct {
	Context *middleware.Context
	Handler DelStationHandler
}

func (o *DelStation) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewDelStationParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// DelStationBody ArgDelStation
//
// swagger:model DelStationBody
type DelStationBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DelStationBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// id
		// Required: true
		ID *int64 `json:"id"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ID = props.ID
	return nil
}

// Validate validates this del station body
func (o *DelStationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DelStationBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this del station body based on context it is used
func (o *DelStationBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *DelStationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DelStationBody) UnmarshalBinary(b []byte) error {
	var res DelStationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
