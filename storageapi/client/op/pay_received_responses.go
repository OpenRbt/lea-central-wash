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

// PayReceivedReader is a Reader for the PayReceived structure.
type PayReceivedReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *PayReceivedReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewPayReceivedNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewPayReceivedUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 404:
		result := NewPayReceivedNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewPayReceivedInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /pay/received] pay_received", response, response.Code())
	}
}

// NewPayReceivedNoContent creates a PayReceivedNoContent with default headers values
func NewPayReceivedNoContent() *PayReceivedNoContent {
	return &PayReceivedNoContent{}
}

/*
PayReceivedNoContent describes a response with status code 204, with default header values.

OK
*/
type PayReceivedNoContent struct {
}

// IsSuccess returns true when this pay received no content response has a 2xx status code
func (o *PayReceivedNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this pay received no content response has a 3xx status code
func (o *PayReceivedNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this pay received no content response has a 4xx status code
func (o *PayReceivedNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this pay received no content response has a 5xx status code
func (o *PayReceivedNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this pay received no content response a status code equal to that given
func (o *PayReceivedNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the pay received no content response
func (o *PayReceivedNoContent) Code() int {
	return 204
}

func (o *PayReceivedNoContent) Error() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedNoContent ", 204)
}

func (o *PayReceivedNoContent) String() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedNoContent ", 204)
}

func (o *PayReceivedNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewPayReceivedUnauthorized creates a PayReceivedUnauthorized with default headers values
func NewPayReceivedUnauthorized() *PayReceivedUnauthorized {
	return &PayReceivedUnauthorized{}
}

/*
PayReceivedUnauthorized describes a response with status code 401, with default header values.

user not authorized
*/
type PayReceivedUnauthorized struct {
}

// IsSuccess returns true when this pay received unauthorized response has a 2xx status code
func (o *PayReceivedUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this pay received unauthorized response has a 3xx status code
func (o *PayReceivedUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this pay received unauthorized response has a 4xx status code
func (o *PayReceivedUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this pay received unauthorized response has a 5xx status code
func (o *PayReceivedUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this pay received unauthorized response a status code equal to that given
func (o *PayReceivedUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the pay received unauthorized response
func (o *PayReceivedUnauthorized) Code() int {
	return 401
}

func (o *PayReceivedUnauthorized) Error() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedUnauthorized ", 401)
}

func (o *PayReceivedUnauthorized) String() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedUnauthorized ", 401)
}

func (o *PayReceivedUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewPayReceivedNotFound creates a PayReceivedNotFound with default headers values
func NewPayReceivedNotFound() *PayReceivedNotFound {
	return &PayReceivedNotFound{}
}

/*
PayReceivedNotFound describes a response with status code 404, with default header values.

hash not found
*/
type PayReceivedNotFound struct {
}

// IsSuccess returns true when this pay received not found response has a 2xx status code
func (o *PayReceivedNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this pay received not found response has a 3xx status code
func (o *PayReceivedNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this pay received not found response has a 4xx status code
func (o *PayReceivedNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this pay received not found response has a 5xx status code
func (o *PayReceivedNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this pay received not found response a status code equal to that given
func (o *PayReceivedNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the pay received not found response
func (o *PayReceivedNotFound) Code() int {
	return 404
}

func (o *PayReceivedNotFound) Error() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedNotFound ", 404)
}

func (o *PayReceivedNotFound) String() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedNotFound ", 404)
}

func (o *PayReceivedNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewPayReceivedInternalServerError creates a PayReceivedInternalServerError with default headers values
func NewPayReceivedInternalServerError() *PayReceivedInternalServerError {
	return &PayReceivedInternalServerError{}
}

/*
PayReceivedInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type PayReceivedInternalServerError struct {
}

// IsSuccess returns true when this pay received internal server error response has a 2xx status code
func (o *PayReceivedInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this pay received internal server error response has a 3xx status code
func (o *PayReceivedInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this pay received internal server error response has a 4xx status code
func (o *PayReceivedInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this pay received internal server error response has a 5xx status code
func (o *PayReceivedInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this pay received internal server error response a status code equal to that given
func (o *PayReceivedInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the pay received internal server error response
func (o *PayReceivedInternalServerError) Code() int {
	return 500
}

func (o *PayReceivedInternalServerError) Error() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedInternalServerError ", 500)
}

func (o *PayReceivedInternalServerError) String() string {
	return fmt.Sprintf("[POST /pay/received][%d] payReceivedInternalServerError ", 500)
}

func (o *PayReceivedInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
PayReceivedBody pay_received
swagger:model PayReceivedBody
*/
type PayReceivedBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`

	// qr order Id
	QrOrderID string `json:"qrOrderId,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *PayReceivedBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`

		// qr order Id
		QrOrderID string `json:"qrOrderId,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.QrOrderID = props.QrOrderID
	return nil
}

// Validate validates this pay received body
func (o *PayReceivedBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *PayReceivedBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this pay received body based on context it is used
func (o *PayReceivedBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *PayReceivedBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *PayReceivedBody) UnmarshalBinary(b []byte) error {
	var res PayReceivedBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
