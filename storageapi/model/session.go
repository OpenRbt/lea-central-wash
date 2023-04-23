// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"

	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// Session session
//
// swagger:model Session
type Session struct {

	// ID
	ID string `json:"ID,omitempty"`

	// q r
	QR string `json:"QR,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *Session) UnmarshalJSON(data []byte) error {
	var props struct {

		// ID
		ID string `json:"ID,omitempty"`

		// q r
		QR string `json:"QR,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.ID = props.ID
	m.QR = props.QR
	return nil
}

// Validate validates this session
func (m *Session) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this session based on context it is used
func (m *Session) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *Session) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *Session) UnmarshalBinary(b []byte) error {
	var res Session
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
