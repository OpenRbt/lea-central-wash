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

// StationConfigVarBool station config var bool
//
// swagger:model StationConfigVarBool
type StationConfigVarBool struct {

	// description
	Description string `json:"description,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// note
	Note string `json:"note,omitempty"`

	// station ID
	StationID int64 `json:"stationID,omitempty"`

	// value
	Value bool `json:"value,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *StationConfigVarBool) UnmarshalJSON(data []byte) error {
	var props struct {

		// description
		Description string `json:"description,omitempty"`

		// name
		Name string `json:"name,omitempty"`

		// note
		Note string `json:"note,omitempty"`

		// station ID
		StationID int64 `json:"stationID,omitempty"`

		// value
		Value bool `json:"value,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Description = props.Description
	m.Name = props.Name
	m.Note = props.Note
	m.StationID = props.StationID
	m.Value = props.Value
	return nil
}

// Validate validates this station config var bool
func (m *StationConfigVarBool) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this station config var bool based on context it is used
func (m *StationConfigVarBool) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *StationConfigVarBool) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *StationConfigVarBool) UnmarshalBinary(b []byte) error {
	var res StationConfigVarBool
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
