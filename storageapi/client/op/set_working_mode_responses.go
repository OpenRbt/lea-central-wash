// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	strfmt "github.com/go-openapi/strfmt"
)

// SetWorkingModeReader is a Reader for the SetWorkingMode structure.
type SetWorkingModeReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetWorkingModeReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 204:
		result := NewSetWorkingModeNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 404:
		result := NewSetWorkingModeNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	case 500:
		result := NewSetWorkingModeInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewSetWorkingModeNoContent creates a SetWorkingModeNoContent with default headers values
func NewSetWorkingModeNoContent() *SetWorkingModeNoContent {
	return &SetWorkingModeNoContent{}
}

/*SetWorkingModeNoContent handles this case with default header values.

OK
*/
type SetWorkingModeNoContent struct {
}

func (o *SetWorkingModeNoContent) Error() string {
	return fmt.Sprintf("[POST /set-working-mode][%d] setWorkingModeNoContent ", 204)
}

func (o *SetWorkingModeNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetWorkingModeNotFound creates a SetWorkingModeNotFound with default headers values
func NewSetWorkingModeNotFound() *SetWorkingModeNotFound {
	return &SetWorkingModeNotFound{}
}

/*SetWorkingModeNotFound handles this case with default header values.

not found
*/
type SetWorkingModeNotFound struct {
}

func (o *SetWorkingModeNotFound) Error() string {
	return fmt.Sprintf("[POST /set-working-mode][%d] setWorkingModeNotFound ", 404)
}

func (o *SetWorkingModeNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetWorkingModeInternalServerError creates a SetWorkingModeInternalServerError with default headers values
func NewSetWorkingModeInternalServerError() *SetWorkingModeInternalServerError {
	return &SetWorkingModeInternalServerError{}
}

/*SetWorkingModeInternalServerError handles this case with default header values.

internal error
*/
type SetWorkingModeInternalServerError struct {
}

func (o *SetWorkingModeInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-working-mode][%d] setWorkingModeInternalServerError ", 500)
}

func (o *SetWorkingModeInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*SetWorkingModeBody set working mode body
swagger:model SetWorkingModeBody
*/
type SetWorkingModeBody struct {

	// service mode
	// Required: true
	ServiceMode *bool `json:"serviceMode"`

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// Validate validates this set working mode body
func (o *SetWorkingModeBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateServiceMode(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SetWorkingModeBody) validateServiceMode(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"serviceMode", "body", o.ServiceMode); err != nil {
		return err
	}

	return nil
}

func (o *SetWorkingModeBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *SetWorkingModeBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SetWorkingModeBody) UnmarshalBinary(b []byte) error {
	var res SetWorkingModeBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
