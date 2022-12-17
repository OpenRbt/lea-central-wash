// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"context"

	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// ConfigVarString config var string
//
// swagger:model ConfigVarString
type ConfigVarString struct {

	// description
	Description string `json:"description,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// note
	Note string `json:"note,omitempty"`

	// value
	Value string `json:"value,omitempty"`
}

// Validate validates this config var string
func (m *ConfigVarString) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this config var string based on context it is used
func (m *ConfigVarString) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *ConfigVarString) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *ConfigVarString) UnmarshalBinary(b []byte) error {
	var res ConfigVarString
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
