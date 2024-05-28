// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// StationStatus station status
//
// swagger:model StationStatus
type StationStatus struct {

	// current balance
	CurrentBalance int64 `json:"currentBalance,omitempty"`

	// current program
	CurrentProgram int64 `json:"currentProgram,omitempty"`

	// current program name
	CurrentProgramName string `json:"currentProgramName,omitempty"`

	// hash
	Hash Hash `json:"hash,omitempty"`

	// id
	ID int64 `json:"id,omitempty"`

	// info
	Info string `json:"info,omitempty"`

	// ip
	IP string `json:"ip,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// status
	Status Status `json:"status,omitempty"`

	// version
	Version *FirmwareVersion `json:"version,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *StationStatus) UnmarshalJSON(data []byte) error {
	var props struct {

		// current balance
		CurrentBalance int64 `json:"currentBalance,omitempty"`

		// current program
		CurrentProgram int64 `json:"currentProgram,omitempty"`

		// current program name
		CurrentProgramName string `json:"currentProgramName,omitempty"`

		// hash
		Hash Hash `json:"hash,omitempty"`

		// id
		ID int64 `json:"id,omitempty"`

		// info
		Info string `json:"info,omitempty"`

		// ip
		IP string `json:"ip,omitempty"`

		// name
		Name string `json:"name,omitempty"`

		// status
		Status Status `json:"status,omitempty"`

		// version
		Version *FirmwareVersion `json:"version,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.CurrentBalance = props.CurrentBalance
	m.CurrentProgram = props.CurrentProgram
	m.CurrentProgramName = props.CurrentProgramName
	m.Hash = props.Hash
	m.ID = props.ID
	m.Info = props.Info
	m.IP = props.IP
	m.Name = props.Name
	m.Status = props.Status
	m.Version = props.Version
	return nil
}

// Validate validates this station status
func (m *StationStatus) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateStatus(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateVersion(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationStatus) validateHash(formats strfmt.Registry) error {
	if swag.IsZero(m.Hash) { // not required
		return nil
	}

	if err := m.Hash.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("hash")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("hash")
		}
		return err
	}

	return nil
}

func (m *StationStatus) validateStatus(formats strfmt.Registry) error {
	if swag.IsZero(m.Status) { // not required
		return nil
	}

	if err := m.Status.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("status")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("status")
		}
		return err
	}

	return nil
}

func (m *StationStatus) validateVersion(formats strfmt.Registry) error {
	if swag.IsZero(m.Version) { // not required
		return nil
	}

	if m.Version != nil {
		if err := m.Version.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("version")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("version")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this station status based on the context it is used
func (m *StationStatus) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := m.contextValidateStatus(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := m.contextValidateVersion(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *StationStatus) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if swag.IsZero(m.Hash) { // not required
		return nil
	}

	if err := m.Hash.ContextValidate(ctx, formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("hash")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("hash")
		}
		return err
	}

	return nil
}

func (m *StationStatus) contextValidateStatus(ctx context.Context, formats strfmt.Registry) error {

	if swag.IsZero(m.Status) { // not required
		return nil
	}

	if err := m.Status.ContextValidate(ctx, formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("status")
		} else if ce, ok := err.(*errors.CompositeError); ok {
			return ce.ValidateName("status")
		}
		return err
	}

	return nil
}

func (m *StationStatus) contextValidateVersion(ctx context.Context, formats strfmt.Registry) error {

	if m.Version != nil {

		if swag.IsZero(m.Version) { // not required
			return nil
		}

		if err := m.Version.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("version")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("version")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (m *StationStatus) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *StationStatus) UnmarshalBinary(b []byte) error {
	var res StationStatus
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
