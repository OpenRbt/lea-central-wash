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

// NewStationEvent new station event
//
// swagger:model NewStationEvent
type NewStationEvent struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`

	// info
	// Required: true
	Info *string `json:"info"`

	// module
	// Required: true
	Module *string `json:"module"`

	// status
	// Required: true
	// Enum: [OK WARNING ERROR CRITICAL]
	Status *string `json:"status"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *NewStationEvent) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`

		// info
		// Required: true
		Info *string `json:"info"`

		// module
		// Required: true
		Module *string `json:"module"`

		// status
		// Required: true
		// Enum: [OK WARNING ERROR CRITICAL]
		Status *string `json:"status"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Hash = props.Hash
	m.Info = props.Info
	m.Module = props.Module
	m.Status = props.Status
	return nil
}

// Validate validates this new station event
func (m *NewStationEvent) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateInfo(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateModule(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateStatus(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *NewStationEvent) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("hash", "body", m.Hash); err != nil {
		return err
	}

	return nil
}

func (m *NewStationEvent) validateInfo(formats strfmt.Registry) error {

	if err := validate.Required("info", "body", m.Info); err != nil {
		return err
	}

	return nil
}

func (m *NewStationEvent) validateModule(formats strfmt.Registry) error {

	if err := validate.Required("module", "body", m.Module); err != nil {
		return err
	}

	return nil
}

var newStationEventTypeStatusPropEnum []interface{}

func init() {
	var res []string
	if err := json.Unmarshal([]byte(`["OK","WARNING","ERROR","CRITICAL"]`), &res); err != nil {
		panic(err)
	}
	for _, v := range res {
		newStationEventTypeStatusPropEnum = append(newStationEventTypeStatusPropEnum, v)
	}
}

const (

	// NewStationEventStatusOK captures enum value "OK"
	NewStationEventStatusOK string = "OK"

	// NewStationEventStatusWARNING captures enum value "WARNING"
	NewStationEventStatusWARNING string = "WARNING"

	// NewStationEventStatusERROR captures enum value "ERROR"
	NewStationEventStatusERROR string = "ERROR"

	// NewStationEventStatusCRITICAL captures enum value "CRITICAL"
	NewStationEventStatusCRITICAL string = "CRITICAL"
)

// prop value enum
func (m *NewStationEvent) validateStatusEnum(path, location string, value string) error {
	if err := validate.EnumCase(path, location, value, newStationEventTypeStatusPropEnum, true); err != nil {
		return err
	}
	return nil
}

func (m *NewStationEvent) validateStatus(formats strfmt.Registry) error {

	if err := validate.Required("status", "body", m.Status); err != nil {
		return err
	}

	// value enum
	if err := m.validateStatusEnum("status", "body", *m.Status); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this new station event based on context it is used
func (m *NewStationEvent) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *NewStationEvent) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *NewStationEvent) UnmarshalBinary(b []byte) error {
	var res NewStationEvent
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
