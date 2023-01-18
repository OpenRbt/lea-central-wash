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
	"github.com/go-openapi/validate"
)

// MoneyReport money report
//
// swagger:model MoneyReport
type MoneyReport struct {

	// banknotes
	Banknotes int64 `json:"banknotes,omitempty"`

	// bonuses
	Bonuses int64 `json:"bonuses,omitempty"`

	// cars total
	CarsTotal int64 `json:"carsTotal,omitempty"`

	// coins
	Coins int64 `json:"coins,omitempty"`

	// electronical
	Electronical int64 `json:"electronical,omitempty"`

	// hash
	// Required: true
	Hash *Hash `json:"hash"`

	// service
	Service int64 `json:"service,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *MoneyReport) UnmarshalJSON(data []byte) error {
	var props struct {

		// banknotes
		Banknotes int64 `json:"banknotes,omitempty"`

		// bonuses
		Bonuses int64 `json:"bonuses,omitempty"`

		// cars total
		CarsTotal int64 `json:"carsTotal,omitempty"`

		// coins
		Coins int64 `json:"coins,omitempty"`

		// electronical
		Electronical int64 `json:"electronical,omitempty"`

		// hash
		// Required: true
		Hash *Hash `json:"hash"`

		// service
		Service int64 `json:"service,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Banknotes = props.Banknotes
	m.Bonuses = props.Bonuses
	m.CarsTotal = props.CarsTotal
	m.Coins = props.Coins
	m.Electronical = props.Electronical
	m.Hash = props.Hash
	m.Service = props.Service
	return nil
}

// Validate validates this money report
func (m *MoneyReport) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *MoneyReport) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("hash", "body", m.Hash); err != nil {
		return err
	}

	if err := validate.Required("hash", "body", m.Hash); err != nil {
		return err
	}

	if m.Hash != nil {
		if err := m.Hash.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("hash")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this money report based on the context it is used
func (m *MoneyReport) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *MoneyReport) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if m.Hash != nil {
		if err := m.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (m *MoneyReport) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *MoneyReport) UnmarshalBinary(b []byte) error {
	var res MoneyReport
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
