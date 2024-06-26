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

// MoneyReportCreation money report creation
//
// swagger:model MoneyReportCreation
type MoneyReportCreation struct {

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

	// qr money
	QrMoney int64 `json:"qrMoney,omitempty"`

	// service
	Service int64 `json:"service,omitempty"`

	// session Id
	SessionID string `json:"sessionId,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *MoneyReportCreation) UnmarshalJSON(data []byte) error {
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

		// qr money
		QrMoney int64 `json:"qrMoney,omitempty"`

		// service
		Service int64 `json:"service,omitempty"`

		// session Id
		SessionID string `json:"sessionId,omitempty"`
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
	m.QrMoney = props.QrMoney
	m.Service = props.Service
	m.SessionID = props.SessionID
	return nil
}

// Validate validates this money report creation
func (m *MoneyReportCreation) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *MoneyReportCreation) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this money report creation based on the context it is used
func (m *MoneyReportCreation) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *MoneyReportCreation) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (m *MoneyReportCreation) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *MoneyReportCreation) UnmarshalBinary(b []byte) error {
	var res MoneyReportCreation
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
