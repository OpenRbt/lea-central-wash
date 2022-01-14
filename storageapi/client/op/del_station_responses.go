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
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
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

func (o *DelStationNoContent) Error() string {
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

func (o *DelStationNotFound) Error() string {
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

func (o *DelStationInternalServerError) Error() string {
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
