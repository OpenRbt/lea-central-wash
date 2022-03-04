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

// CardReaderConfigHandlerFunc turns a function with the right signature into a card reader config handler
type CardReaderConfigHandlerFunc func(CardReaderConfigParams) CardReaderConfigResponder

// Handle executing the request and returning a response
func (fn CardReaderConfigHandlerFunc) Handle(params CardReaderConfigParams) CardReaderConfigResponder {
	return fn(params)
}

// CardReaderConfigHandler interface for that can handle valid card reader config params
type CardReaderConfigHandler interface {
	Handle(CardReaderConfigParams) CardReaderConfigResponder
}

// NewCardReaderConfig creates a new http.Handler for the card reader config operation
func NewCardReaderConfig(ctx *middleware.Context, handler CardReaderConfigHandler) *CardReaderConfig {
	return &CardReaderConfig{Context: ctx, Handler: handler}
}

/* CardReaderConfig swagger:route POST /card-reader-config cardReaderConfig

CardReaderConfig card reader config API

*/
type CardReaderConfig struct {
	Context *middleware.Context
	Handler CardReaderConfigHandler
}

func (o *CardReaderConfig) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewCardReaderConfigParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// CardReaderConfigBody ArgCardReaderConfig
//
// swagger:model CardReaderConfigBody
type CardReaderConfigBody struct {

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CardReaderConfigBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// station ID
		// Required: true
		// Minimum: 1
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

// Validate validates this card reader config body
func (o *CardReaderConfigBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CardReaderConfigBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this card reader config body based on context it is used
func (o *CardReaderConfigBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *CardReaderConfigBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CardReaderConfigBody) UnmarshalBinary(b []byte) error {
	var res CardReaderConfigBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
