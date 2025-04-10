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

// Log log
//
// swagger:model Log
type Log struct {

	// hash
	// Required: true
	Hash *Hash `json:"hash"`

	// level
	// Enum: ["debug","info","warning","error"]
	Level *string `json:"level,omitempty"`

	// text
	// Required: true
	Text *string `json:"text"`

	// type
	Type *string `json:"type,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *Log) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *Hash `json:"hash"`

		// level
		// Enum: ["debug","info","warning","error"]
		Level *string `json:"level,omitempty"`

		// text
		// Required: true
		Text *string `json:"text"`

		// type
		Type *string `json:"type,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Hash = props.Hash
	m.Level = props.Level
	m.Text = props.Text
	m.Type = props.Type
	return nil
}

// Validate validates this log
func (m *Log) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateLevel(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateText(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *Log) validateHash(formats strfmt.Registry) error {

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

var logTypeLevelPropEnum []interface{}

func init() {
	var res []string
	if err := json.Unmarshal([]byte(`["debug","info","warning","error"]`), &res); err != nil {
		panic(err)
	}
	for _, v := range res {
		logTypeLevelPropEnum = append(logTypeLevelPropEnum, v)
	}
}

const (

	// LogLevelDebug captures enum value "debug"
	LogLevelDebug string = "debug"

	// LogLevelInfo captures enum value "info"
	LogLevelInfo string = "info"

	// LogLevelWarning captures enum value "warning"
	LogLevelWarning string = "warning"

	// LogLevelError captures enum value "error"
	LogLevelError string = "error"
)

// prop value enum
func (m *Log) validateLevelEnum(path, location string, value string) error {
	if err := validate.EnumCase(path, location, value, logTypeLevelPropEnum, true); err != nil {
		return err
	}
	return nil
}

func (m *Log) validateLevel(formats strfmt.Registry) error {
	if swag.IsZero(m.Level) { // not required
		return nil
	}

	// value enum
	if err := m.validateLevelEnum("level", "body", *m.Level); err != nil {
		return err
	}

	return nil
}

func (m *Log) validateText(formats strfmt.Registry) error {

	if err := validate.Required("text", "body", m.Text); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this log based on the context it is used
func (m *Log) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *Log) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (m *Log) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *Log) UnmarshalBinary(b []byte) error {
	var res Log
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
