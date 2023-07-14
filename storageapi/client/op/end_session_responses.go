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

// EndSessionReader is a Reader for the EndSession structure.
type EndSessionReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *EndSessionReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewEndSessionNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewEndSessionNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewEndSessionInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /end-session] endSession", response, response.Code())
	}
}

// NewEndSessionNoContent creates a EndSessionNoContent with default headers values
func NewEndSessionNoContent() *EndSessionNoContent {
	return &EndSessionNoContent{}
}

/*
EndSessionNoContent describes a response with status code 204, with default header values.

OK
*/
type EndSessionNoContent struct {
}

// IsSuccess returns true when this end session no content response has a 2xx status code
func (o *EndSessionNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this end session no content response has a 3xx status code
func (o *EndSessionNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this end session no content response has a 4xx status code
func (o *EndSessionNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this end session no content response has a 5xx status code
func (o *EndSessionNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this end session no content response a status code equal to that given
func (o *EndSessionNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the end session no content response
func (o *EndSessionNoContent) Code() int {
	return 204
}

func (o *EndSessionNoContent) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionNoContent ", 204)
}

func (o *EndSessionNoContent) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionNoContent ", 204)
}

func (o *EndSessionNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEndSessionNotFound creates a EndSessionNotFound with default headers values
func NewEndSessionNotFound() *EndSessionNotFound {
	return &EndSessionNotFound{}
}

/*
EndSessionNotFound describes a response with status code 404, with default header values.

hash not found
*/
type EndSessionNotFound struct {
}

// IsSuccess returns true when this end session not found response has a 2xx status code
func (o *EndSessionNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this end session not found response has a 3xx status code
func (o *EndSessionNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this end session not found response has a 4xx status code
func (o *EndSessionNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this end session not found response has a 5xx status code
func (o *EndSessionNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this end session not found response a status code equal to that given
func (o *EndSessionNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the end session not found response
func (o *EndSessionNotFound) Code() int {
	return 404
}

func (o *EndSessionNotFound) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionNotFound ", 404)
}

func (o *EndSessionNotFound) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionNotFound ", 404)
}

func (o *EndSessionNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEndSessionInternalServerError creates a EndSessionInternalServerError with default headers values
func NewEndSessionInternalServerError() *EndSessionInternalServerError {
	return &EndSessionInternalServerError{}
}

/*
EndSessionInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type EndSessionInternalServerError struct {
}

// IsSuccess returns true when this end session internal server error response has a 2xx status code
func (o *EndSessionInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this end session internal server error response has a 3xx status code
func (o *EndSessionInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this end session internal server error response has a 4xx status code
func (o *EndSessionInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this end session internal server error response has a 5xx status code
func (o *EndSessionInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this end session internal server error response a status code equal to that given
func (o *EndSessionInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the end session internal server error response
func (o *EndSessionInternalServerError) Code() int {
	return 500
}

func (o *EndSessionInternalServerError) Error() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionInternalServerError ", 500)
}

func (o *EndSessionInternalServerError) String() string {
	return fmt.Sprintf("[POST /end-session][%d] endSessionInternalServerError ", 500)
}

func (o *EndSessionInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
EndSessionBody ArgEndSession
swagger:model EndSessionBody
*/
type EndSessionBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`

	// session ID
	// Required: true
	SessionID *string `json:"sessionID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *EndSessionBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`

		// session ID
		// Required: true
		SessionID *string `json:"sessionID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.SessionID = props.SessionID
	return nil
}

// Validate validates this end session body
func (o *EndSessionBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateSessionID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *EndSessionBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

func (o *EndSessionBody) validateSessionID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"sessionID", "body", o.SessionID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this end session body based on context it is used
func (o *EndSessionBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *EndSessionBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *EndSessionBody) UnmarshalBinary(b []byte) error {
	var res EndSessionBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
