// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// SetStationButtonHandlerFunc turns a function with the right signature into a set station button handler
type SetStationButtonHandlerFunc func(SetStationButtonParams) SetStationButtonResponder

// Handle executing the request and returning a response
func (fn SetStationButtonHandlerFunc) Handle(params SetStationButtonParams) SetStationButtonResponder {
	return fn(params)
}

// SetStationButtonHandler interface for that can handle valid set station button params
type SetStationButtonHandler interface {
	Handle(SetStationButtonParams) SetStationButtonResponder
}

// NewSetStationButton creates a new http.Handler for the set station button operation
func NewSetStationButton(ctx *middleware.Context, handler SetStationButtonHandler) *SetStationButton {
	return &SetStationButton{Context: ctx, Handler: handler}
}

/* SetStationButton swagger:route POST /set-station-button setStationButton

SetStationButton set station button API

*/
type SetStationButton struct {
	Context *middleware.Context
	Handler SetStationButtonHandler
}

func (o *SetStationButton) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewSetStationButtonParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// SetStationButtonBody ArgSetStationButton
//
// swagger:model SetStationButtonBody
type SetStationButtonBody struct {

	// buttons
	Buttons []*SetStationButtonParamsBodyButtonsItems0 `json:"buttons"`

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SetStationButtonBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// buttons
		Buttons []*SetStationButtonParamsBodyButtonsItems0 `json:"buttons"`

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

	o.Buttons = props.Buttons
	o.StationID = props.StationID
	return nil
}

// Validate validates this set station button body
func (o *SetStationButtonBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateButtons(formats); err != nil {
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

func (o *SetStationButtonBody) validateButtons(formats strfmt.Registry) error {
	if swag.IsZero(o.Buttons) { // not required
		return nil
	}

	for i := 0; i < len(o.Buttons); i++ {
		if swag.IsZero(o.Buttons[i]) { // not required
			continue
		}

		if o.Buttons[i] != nil {
			if err := o.Buttons[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (o *SetStationButtonBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this set station button body based on the context it is used
func (o *SetStationButtonBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateButtons(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SetStationButtonBody) contextValidateButtons(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(o.Buttons); i++ {

		if o.Buttons[i] != nil {
			if err := o.Buttons[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (o *SetStationButtonBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SetStationButtonBody) UnmarshalBinary(b []byte) error {
	var res SetStationButtonBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// SetStationButtonParamsBodyButtonsItems0 set station button params body buttons items0
//
// swagger:model SetStationButtonParamsBodyButtonsItems0
type SetStationButtonParamsBodyButtonsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program ID
	ProgramID int64 `json:"programID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SetStationButtonParamsBodyButtonsItems0) UnmarshalJSON(data []byte) error {
	var props struct {

		// button ID
		ButtonID int64 `json:"buttonID,omitempty"`

		// program ID
		ProgramID int64 `json:"programID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ButtonID = props.ButtonID
	o.ProgramID = props.ProgramID
	return nil
}

// Validate validates this set station button params body buttons items0
func (o *SetStationButtonParamsBodyButtonsItems0) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this set station button params body buttons items0 based on context it is used
func (o *SetStationButtonParamsBodyButtonsItems0) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *SetStationButtonParamsBodyButtonsItems0) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SetStationButtonParamsBodyButtonsItems0) UnmarshalBinary(b []byte) error {
	var res SetStationButtonParamsBodyButtonsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
