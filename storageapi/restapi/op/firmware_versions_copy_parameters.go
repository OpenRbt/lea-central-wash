// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// NewFirmwareVersionsCopyParams creates a new FirmwareVersionsCopyParams object
//
// There are no default values defined in the spec.
func NewFirmwareVersionsCopyParams() FirmwareVersionsCopyParams {

	return FirmwareVersionsCopyParams{}
}

// FirmwareVersionsCopyParams contains all the bound params for the firmware versions copy operation
// typically these are obtained from a http.Request
//
// swagger:parameters firmwareVersionsCopy
type FirmwareVersionsCopyParams struct {

	// HTTP Request Object
	HTTPRequest *http.Request `json:"-"`

	/*
	  Required: true
	  Minimum: 1
	  In: path
	*/
	ID int64
	/*
	  Required: true
	  Minimum: 1
	  In: path
	*/
	ToID int64
}

// BindRequest both binds and validates a request, it assumes that complex things implement a Validatable(strfmt.Registry) error interface
// for simple values it will use straight method calls.
//
// To ensure default values, the struct must have been initialized with NewFirmwareVersionsCopyParams() beforehand.
func (o *FirmwareVersionsCopyParams) BindRequest(r *http.Request, route *middleware.MatchedRoute) error {
	var res []error

	o.HTTPRequest = r

	rID, rhkID, _ := route.Params.GetOK("id")
	if err := o.bindID(rID, rhkID, route.Formats); err != nil {
		res = append(res, err)
	}

	rToID, rhkToID, _ := route.Params.GetOK("toID")
	if err := o.bindToID(rToID, rhkToID, route.Formats); err != nil {
		res = append(res, err)
	}
	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

// bindID binds and validates parameter ID from path.
func (o *FirmwareVersionsCopyParams) bindID(rawData []string, hasKey bool, formats strfmt.Registry) error {
	var raw string
	if len(rawData) > 0 {
		raw = rawData[len(rawData)-1]
	}

	// Required: true
	// Parameter is provided by construction from the route

	value, err := swag.ConvertInt64(raw)
	if err != nil {
		return errors.InvalidType("id", "path", "int64", raw)
	}
	o.ID = value

	if err := o.validateID(formats); err != nil {
		return err
	}

	return nil
}

// validateID carries on validations for parameter ID
func (o *FirmwareVersionsCopyParams) validateID(formats strfmt.Registry) error {

	if err := validate.MinimumInt("id", "path", o.ID, 1, false); err != nil {
		return err
	}

	return nil
}

// bindToID binds and validates parameter ToID from path.
func (o *FirmwareVersionsCopyParams) bindToID(rawData []string, hasKey bool, formats strfmt.Registry) error {
	var raw string
	if len(rawData) > 0 {
		raw = rawData[len(rawData)-1]
	}

	// Required: true
	// Parameter is provided by construction from the route

	value, err := swag.ConvertInt64(raw)
	if err != nil {
		return errors.InvalidType("toID", "path", "int64", raw)
	}
	o.ToID = value

	if err := o.validateToID(formats); err != nil {
		return err
	}

	return nil
}

// validateToID carries on validations for parameter ToID
func (o *FirmwareVersionsCopyParams) validateToID(formats strfmt.Registry) error {

	if err := validate.MinimumInt("toID", "path", o.ToID, 1, false); err != nil {
		return err
	}

	return nil
}
