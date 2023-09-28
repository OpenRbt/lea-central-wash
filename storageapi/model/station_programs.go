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
)

// StationPrograms station programs
//
// swagger:model StationPrograms
type StationPrograms struct {

	// last update
	LastUpdate int64 `json:"lastUpdate,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// preflight sec
	PreflightSec int64 `json:"preflightSec,omitempty"`

	// programs
	Programs []*StationProgramsProgramsItems0 `json:"programs"`

	// relay board
	RelayBoard RelayBoard `json:"relayBoard,omitempty"`

	// station ID
	StationID int64 `json:"stationID,omitempty"`
}

// Validate validates this station programs
func (m *StationPrograms) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validatePrograms(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateRelayBoard(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationPrograms) validatePrograms(formats strfmt.Registry) error {
	if swag.IsZero(m.Programs) { // not required
		return nil
	}

	for i := 0; i < len(m.Programs); i++ {
		if swag.IsZero(m.Programs[i]) { // not required
			continue
		}

		if m.Programs[i] != nil {
			if err := m.Programs[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("programs" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("programs" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (m *StationPrograms) validateRelayBoard(formats strfmt.Registry) error {
	if swag.IsZero(m.RelayBoard) { // not required
		return nil
	}

	if err := m.RelayBoard.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("relayBoard")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("relayBoard")
		}
		return err
	}

	return nil
}

// ContextValidate validate this station programs based on the context it is used
func (m *StationPrograms) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidatePrograms(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := m.contextValidateRelayBoard(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationPrograms) contextValidatePrograms(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(m.Programs); i++ {

		if m.Programs[i] != nil {

			if swag.IsZero(m.Programs[i]) { // not required
				return nil
			}

			if err := m.Programs[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("programs" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("programs" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (m *StationPrograms) contextValidateRelayBoard(ctx context.Context, formats strfmt.Registry) error {

	if swag.IsZero(m.RelayBoard) { // not required
		return nil
	}

	if err := m.RelayBoard.ContextValidate(ctx, formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("relayBoard")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("relayBoard")
		}
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (m *StationPrograms) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *StationPrograms) UnmarshalBinary(b []byte) error {
	var res StationPrograms
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}

// StationProgramsProgramsItems0 station programs programs items0
//
// swagger:model StationProgramsProgramsItems0
type StationProgramsProgramsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program
	Program *Program `json:"program,omitempty"`
}

// Validate validates this station programs programs items0
func (m *StationProgramsProgramsItems0) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateProgram(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationProgramsProgramsItems0) validateProgram(formats strfmt.Registry) error {
	if swag.IsZero(m.Program) { // not required
		return nil
	}

	if m.Program != nil {
		if err := m.Program.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("program")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("program")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this station programs programs items0 based on the context it is used
func (m *StationProgramsProgramsItems0) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateProgram(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationProgramsProgramsItems0) contextValidateProgram(ctx context.Context, formats strfmt.Registry) error {

	if m.Program != nil {

		if swag.IsZero(m.Program) { // not required
			return nil
		}

		if err := m.Program.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("program")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("program")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (m *StationProgramsProgramsItems0) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *StationProgramsProgramsItems0) UnmarshalBinary(b []byte) error {
	var res StationProgramsProgramsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
