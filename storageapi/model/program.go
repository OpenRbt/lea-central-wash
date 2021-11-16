// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"context"
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// Program program
//
// swagger:model Program
type Program struct {

	// id
	// Required: true
	// Minimum: 1
	ID *int64 `json:"id"`

	// is finishing program
	IsFinishingProgram bool `json:"isFinishingProgram,omitempty"`

	// motor speed percent
	// Maximum: 100
	// Minimum: 0
	MotorSpeedPercent *int64 `json:"motorSpeedPercent,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// preflight enabled
	PreflightEnabled bool `json:"preflightEnabled,omitempty"`

	// preflight motor speed percent
	// Maximum: 100
	// Minimum: 0
	PreflightMotorSpeedPercent *int64 `json:"preflightMotorSpeedPercent,omitempty"`

	// preflight relays
	PreflightRelays []*RelayConfig `json:"preflightRelays"`

	// price
	Price int64 `json:"price,omitempty"`

	// relays
	Relays []*RelayConfig `json:"relays"`
}

// Validate validates this program
func (m *Program) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateID(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateMotorSpeedPercent(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validatePreflightMotorSpeedPercent(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validatePreflightRelays(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateRelays(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *Program) validateID(formats strfmt.Registry) error {

	if err := validate.Required("id", "body", m.ID); err != nil {
		return err
	}

	if err := validate.MinimumInt("id", "body", *m.ID, 1, false); err != nil {
		return err
	}

	return nil
}

func (m *Program) validateMotorSpeedPercent(formats strfmt.Registry) error {
	if swag.IsZero(m.MotorSpeedPercent) { // not required
		return nil
	}

	if err := validate.MinimumInt("motorSpeedPercent", "body", *m.MotorSpeedPercent, 0, false); err != nil {
		return err
	}

	if err := validate.MaximumInt("motorSpeedPercent", "body", *m.MotorSpeedPercent, 100, false); err != nil {
		return err
	}

	return nil
}

func (m *Program) validatePreflightMotorSpeedPercent(formats strfmt.Registry) error {
	if swag.IsZero(m.PreflightMotorSpeedPercent) { // not required
		return nil
	}

	if err := validate.MinimumInt("preflightMotorSpeedPercent", "body", *m.PreflightMotorSpeedPercent, 0, false); err != nil {
		return err
	}

	if err := validate.MaximumInt("preflightMotorSpeedPercent", "body", *m.PreflightMotorSpeedPercent, 100, false); err != nil {
		return err
	}

	return nil
}

func (m *Program) validatePreflightRelays(formats strfmt.Registry) error {
	if swag.IsZero(m.PreflightRelays) { // not required
		return nil
	}

	for i := 0; i < len(m.PreflightRelays); i++ {
		if swag.IsZero(m.PreflightRelays[i]) { // not required
			continue
		}

		if m.PreflightRelays[i] != nil {
			if err := m.PreflightRelays[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("preflightRelays" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (m *Program) validateRelays(formats strfmt.Registry) error {
	if swag.IsZero(m.Relays) { // not required
		return nil
	}

	for i := 0; i < len(m.Relays); i++ {
		if swag.IsZero(m.Relays[i]) { // not required
			continue
		}

		if m.Relays[i] != nil {
			if err := m.Relays[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("relays" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// ContextValidate validate this program based on the context it is used
func (m *Program) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidatePreflightRelays(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := m.contextValidateRelays(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *Program) contextValidatePreflightRelays(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(m.PreflightRelays); i++ {

		if m.PreflightRelays[i] != nil {
			if err := m.PreflightRelays[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("preflightRelays" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (m *Program) contextValidateRelays(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(m.Relays); i++ {

		if m.Relays[i] != nil {
			if err := m.Relays[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("relays" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (m *Program) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *Program) UnmarshalBinary(b []byte) error {
	var res Program
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
