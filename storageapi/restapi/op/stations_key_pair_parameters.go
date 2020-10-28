// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
)

// NewStationsKeyPairParams creates a new StationsKeyPairParams object
// no default values defined in spec.
func NewStationsKeyPairParams() StationsKeyPairParams {

	return StationsKeyPairParams{}
}

// StationsKeyPairParams contains all the bound params for the stations key pair operation
// typically these are obtained from a http.Request
//
// swagger:parameters stationsKeyPair
type StationsKeyPairParams struct {

	// HTTP Request Object
	HTTPRequest *http.Request `json:"-"`
}

// BindRequest both binds and validates a request, it assumes that complex things implement a Validatable(strfmt.Registry) error interface
// for simple values it will use straight method calls.
//
// To ensure default values, the struct must have been initialized with NewStationsKeyPairParams() beforehand.
func (o *StationsKeyPairParams) BindRequest(r *http.Request, route *middleware.MatchedRoute) error {
	var res []error

	o.HTTPRequest = r

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}
