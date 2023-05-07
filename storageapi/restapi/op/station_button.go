// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"context"
	"net/http"
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// StationButtonHandlerFunc turns a function with the right signature into a station button handler
type StationButtonHandlerFunc func(StationButtonParams) StationButtonResponder

// Handle executing the request and returning a response
func (fn StationButtonHandlerFunc) Handle(params StationButtonParams) StationButtonResponder {
	return fn(params)
}

// StationButtonHandler interface for that can handle valid station button params
type StationButtonHandler interface {
	Handle(StationButtonParams) StationButtonResponder
}

// NewStationButton creates a new http.Handler for the station button operation
func NewStationButton(ctx *middleware.Context, handler StationButtonHandler) *StationButton {
	return &StationButton{Context: ctx, Handler: handler}
}

/*
	StationButton swagger:route POST /station-button stationButton

StationButton station button API
*/
type StationButton struct {
	Context *middleware.Context
	Handler StationButtonHandler
}

func (o *StationButton) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewStationButtonParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// StationButtonBody ArgStationButton
//
// swagger:model StationButtonBody
type StationButtonBody struct {

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// Validate validates this station button body
func (o *StationButtonBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station button body based on context it is used
func (o *StationButtonBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonBody) UnmarshalBinary(b []byte) error {
	var res StationButtonBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// StationButtonOKBody ResponseStationButton
//
// swagger:model StationButtonOKBody
type StationButtonOKBody struct {

	// buttons
	Buttons []*StationButtonOKBodyButtonsItems0 `json:"buttons"`
}

// Validate validates this station button o k body
func (o *StationButtonOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateButtons(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonOKBody) validateButtons(formats strfmt.Registry) error {
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
					return ve.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// ContextValidate validate this station button o k body based on the context it is used
func (o *StationButtonOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateButtons(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonOKBody) contextValidateButtons(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(o.Buttons); i++ {

		if o.Buttons[i] != nil {
			if err := o.Buttons[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonOKBody) UnmarshalBinary(b []byte) error {
	var res StationButtonOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// StationButtonOKBodyButtonsItems0 station button o k body buttons items0
//
// swagger:model StationButtonOKBodyButtonsItems0
type StationButtonOKBodyButtonsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program ID
	ProgramID int64 `json:"programID,omitempty"`
}

// Validate validates this station button o k body buttons items0
func (o *StationButtonOKBodyButtonsItems0) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this station button o k body buttons items0 based on context it is used
func (o *StationButtonOKBodyButtonsItems0) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonOKBodyButtonsItems0) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonOKBodyButtonsItems0) UnmarshalBinary(b []byte) error {
	var res StationButtonOKBodyButtonsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
