// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// DelStationReader is a Reader for the DelStation structure.
type DelStationReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *DelStationReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewDelStationNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewDelStationNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewDelStationInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /del-station] delStation", response, response.Code())
	}
}

// NewDelStationNoContent creates a DelStationNoContent with default headers values
func NewDelStationNoContent() *DelStationNoContent {
	return &DelStationNoContent{}
}

/* DelStationNoContent describes a response with status code 204, with default header values.

OK
*/
type DelStationNoContent struct {
}

// IsSuccess returns true when this del station no content response has a 2xx status code
func (o *DelStationNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this del station no content response has a 3xx status code
func (o *DelStationNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del station no content response has a 4xx status code
func (o *DelStationNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this del station no content response has a 5xx status code
func (o *DelStationNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this del station no content response a status code equal to that given
func (o *DelStationNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the del station no content response
func (o *DelStationNoContent) Code() int {
	return 204
}

func (o *DelStationNoContent) Error() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationNoContent ", 204)
}

func (o *DelStationNoContent) String() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationNoContent ", 204)
}

func (o *DelStationNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDelStationNotFound creates a DelStationNotFound with default headers values
func NewDelStationNotFound() *DelStationNotFound {
	return &DelStationNotFound{}
}

/* DelStationNotFound describes a response with status code 404, with default header values.

not found
*/
type DelStationNotFound struct {
}

// IsSuccess returns true when this del station not found response has a 2xx status code
func (o *DelStationNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this del station not found response has a 3xx status code
func (o *DelStationNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del station not found response has a 4xx status code
func (o *DelStationNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this del station not found response has a 5xx status code
func (o *DelStationNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this del station not found response a status code equal to that given
func (o *DelStationNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the del station not found response
func (o *DelStationNotFound) Code() int {
	return 404
}

func (o *DelStationNotFound) Error() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationNotFound ", 404)
}

func (o *DelStationNotFound) String() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationNotFound ", 404)
}

func (o *DelStationNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDelStationInternalServerError creates a DelStationInternalServerError with default headers values
func NewDelStationInternalServerError() *DelStationInternalServerError {
	return &DelStationInternalServerError{}
}

/* DelStationInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type DelStationInternalServerError struct {
}

// IsSuccess returns true when this del station internal server error response has a 2xx status code
func (o *DelStationInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this del station internal server error response has a 3xx status code
func (o *DelStationInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del station internal server error response has a 4xx status code
func (o *DelStationInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this del station internal server error response has a 5xx status code
func (o *DelStationInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this del station internal server error response a status code equal to that given
func (o *DelStationInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the del station internal server error response
func (o *DelStationInternalServerError) Code() int {
	return 500
}

func (o *DelStationInternalServerError) Error() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationInternalServerError ", 500)
}

func (o *DelStationInternalServerError) String() string {
	return fmt.Sprintf("[POST /del-station][%d] delStationInternalServerError ", 500)
}

func (o *DelStationInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*DelStationBody ArgDelStation
swagger:model DelStationBody
*/
type DelStationBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DelStationBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// id
		// Required: true
		ID *int64 `json:"id"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ID = props.ID
	return nil
}

// Validate validates this del station body
func (o *DelStationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DelStationBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this del station body based on context it is used
func (o *DelStationBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *DelStationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DelStationBody) UnmarshalBinary(b []byte) error {
	var res DelStationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
