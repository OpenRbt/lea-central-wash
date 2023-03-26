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

// IsAuthorized is authorized
//
// swagger:model IsAuthorized
type IsAuthorized struct {

	// authorized
	Authorized bool `json:"authorized,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *IsAuthorized) UnmarshalJSON(data []byte) error {
	var props struct {

		// authorized
		Authorized bool `json:"authorized,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Authorized = props.Authorized
	return nil
}

// Validate validates this is authorized
func (m *IsAuthorized) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this is authorized based on context it is used
func (m *IsAuthorized) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *IsAuthorized) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *IsAuthorized) UnmarshalBinary(b []byte) error {
	var res IsAuthorized
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}