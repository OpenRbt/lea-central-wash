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

// OpenStationReader is a Reader for the OpenStation structure.
type OpenStationReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *OpenStationReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 204:
		result := NewOpenStationNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 404:
		result := NewOpenStationNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	case 500:
		result := NewOpenStationInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewOpenStationNoContent creates a OpenStationNoContent with default headers values
func NewOpenStationNoContent() *OpenStationNoContent {
	return &OpenStationNoContent{}
}

/*OpenStationNoContent handles this case with default header values.

OK
*/
type OpenStationNoContent struct {
}

func (o *OpenStationNoContent) Error() string {
	return fmt.Sprintf("[POST /open-station][%d] openStationNoContent ", 204)
}

func (o *OpenStationNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewOpenStationNotFound creates a OpenStationNotFound with default headers values
func NewOpenStationNotFound() *OpenStationNotFound {
	return &OpenStationNotFound{}
}

/*OpenStationNotFound handles this case with default header values.

not found
*/
type OpenStationNotFound struct {
}

func (o *OpenStationNotFound) Error() string {
	return fmt.Sprintf("[POST /open-station][%d] openStationNotFound ", 404)
}

func (o *OpenStationNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewOpenStationInternalServerError creates a OpenStationInternalServerError with default headers values
func NewOpenStationInternalServerError() *OpenStationInternalServerError {
	return &OpenStationInternalServerError{}
}

/*OpenStationInternalServerError handles this case with default header values.

internal error
*/
type OpenStationInternalServerError struct {
}

func (o *OpenStationInternalServerError) Error() string {
	return fmt.Sprintf("[POST /open-station][%d] openStationInternalServerError ", 500)
}

func (o *OpenStationInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*OpenStationBody open station body
swagger:model OpenStationBody
*/
type OpenStationBody struct {

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// Validate validates this open station body
func (o *OpenStationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *OpenStationBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *OpenStationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *OpenStationBody) UnmarshalBinary(b []byte) error {
	var res OpenStationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
