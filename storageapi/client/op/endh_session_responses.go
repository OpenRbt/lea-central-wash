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

// EndhSessionReader is a Reader for the EndhSession structure.
type EndhSessionReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *EndhSessionReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewEndhSessionNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewEndhSessionNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewEndhSessionInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewEndhSessionNoContent creates a EndhSessionNoContent with default headers values
func NewEndhSessionNoContent() *EndhSessionNoContent {
	return &EndhSessionNoContent{}
}

/*
EndhSessionNoContent describes a response with status code 204, with default header values.

OK
*/
type EndhSessionNoContent struct {
}

// IsSuccess returns true when this endh session no content response has a 2xx status code
func (o *EndhSessionNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this endh session no content response has a 3xx status code
func (o *EndhSessionNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this endh session no content response has a 4xx status code
func (o *EndhSessionNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this endh session no content response has a 5xx status code
func (o *EndhSessionNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this endh session no content response a status code equal to that given
func (o *EndhSessionNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *EndhSessionNoContent) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionNoContent ", 204)
}

func (o *EndhSessionNoContent) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionNoContent ", 204)
}

func (o *EndhSessionNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEndhSessionNotFound creates a EndhSessionNotFound with default headers values
func NewEndhSessionNotFound() *EndhSessionNotFound {
	return &EndhSessionNotFound{}
}

/*
EndhSessionNotFound describes a response with status code 404, with default header values.

hash not found
*/
type EndhSessionNotFound struct {
}

// IsSuccess returns true when this endh session not found response has a 2xx status code
func (o *EndhSessionNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this endh session not found response has a 3xx status code
func (o *EndhSessionNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this endh session not found response has a 4xx status code
func (o *EndhSessionNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this endh session not found response has a 5xx status code
func (o *EndhSessionNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this endh session not found response a status code equal to that given
func (o *EndhSessionNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *EndhSessionNotFound) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionNotFound ", 404)
}

func (o *EndhSessionNotFound) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionNotFound ", 404)
}

func (o *EndhSessionNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEndhSessionInternalServerError creates a EndhSessionInternalServerError with default headers values
func NewEndhSessionInternalServerError() *EndhSessionInternalServerError {
	return &EndhSessionInternalServerError{}
}

/*
EndhSessionInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type EndhSessionInternalServerError struct {
}

// IsSuccess returns true when this endh session internal server error response has a 2xx status code
func (o *EndhSessionInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this endh session internal server error response has a 3xx status code
func (o *EndhSessionInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this endh session internal server error response has a 4xx status code
func (o *EndhSessionInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this endh session internal server error response has a 5xx status code
func (o *EndhSessionInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this endh session internal server error response a status code equal to that given
func (o *EndhSessionInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *EndhSessionInternalServerError) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionInternalServerError ", 500)
}

func (o *EndhSessionInternalServerError) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endhSessionInternalServerError ", 500)
}

func (o *EndhSessionInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
EndhSessionBody ArgEndSession
swagger:model EndhSessionBody
*/
type EndhSessionBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *EndhSessionBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	return nil
}

// Validate validates this endh session body
func (o *EndhSessionBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *EndhSessionBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this endh session body based on context it is used
func (o *EndhSessionBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *EndhSessionBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *EndhSessionBody) UnmarshalBinary(b []byte) error {
	var res EndhSessionBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
