// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"context"

	"github.com/go-openapi/strfmt"
)

// IsAdmin is admin
//
// swagger:model IsAdmin
type IsAdmin bool

// Validate validates this is admin
func (m IsAdmin) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this is admin based on context it is used
func (m IsAdmin) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}
