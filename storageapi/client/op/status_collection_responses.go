// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StatusCollectionReader is a Reader for the StatusCollection structure.
type StatusCollectionReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StatusCollectionReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStatusCollectionOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewStatusCollectionUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewStatusCollectionInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[GET /status-collection] statusCollection", response, response.Code())
	}
}

// NewStatusCollectionOK creates a StatusCollectionOK with default headers values
func NewStatusCollectionOK() *StatusCollectionOK {
	return &StatusCollectionOK{}
}

/*
StatusCollectionOK describes a response with status code 200, with default header values.

OK
*/
type StatusCollectionOK struct {
	Payload *model.StatusCollectionReport
}

// IsSuccess returns true when this status collection o k response has a 2xx status code
func (o *StatusCollectionOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this status collection o k response has a 3xx status code
func (o *StatusCollectionOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this status collection o k response has a 4xx status code
func (o *StatusCollectionOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this status collection o k response has a 5xx status code
func (o *StatusCollectionOK) IsServerError() bool {
	return false
}

// IsCode returns true when this status collection o k response a status code equal to that given
func (o *StatusCollectionOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the status collection o k response
func (o *StatusCollectionOK) Code() int {
	return 200
}

func (o *StatusCollectionOK) Error() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionOK  %+v", 200, o.Payload)
}

func (o *StatusCollectionOK) String() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionOK  %+v", 200, o.Payload)
}

func (o *StatusCollectionOK) GetPayload() *model.StatusCollectionReport {
	return o.Payload
}

func (o *StatusCollectionOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StatusCollectionReport)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStatusCollectionUnauthorized creates a StatusCollectionUnauthorized with default headers values
func NewStatusCollectionUnauthorized() *StatusCollectionUnauthorized {
	return &StatusCollectionUnauthorized{}
}

/*
StatusCollectionUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type StatusCollectionUnauthorized struct {
}

// IsSuccess returns true when this status collection unauthorized response has a 2xx status code
func (o *StatusCollectionUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this status collection unauthorized response has a 3xx status code
func (o *StatusCollectionUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this status collection unauthorized response has a 4xx status code
func (o *StatusCollectionUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this status collection unauthorized response has a 5xx status code
func (o *StatusCollectionUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this status collection unauthorized response a status code equal to that given
func (o *StatusCollectionUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the status collection unauthorized response
func (o *StatusCollectionUnauthorized) Code() int {
	return 401
}

func (o *StatusCollectionUnauthorized) Error() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionUnauthorized ", 401)
}

func (o *StatusCollectionUnauthorized) String() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionUnauthorized ", 401)
}

func (o *StatusCollectionUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStatusCollectionInternalServerError creates a StatusCollectionInternalServerError with default headers values
func NewStatusCollectionInternalServerError() *StatusCollectionInternalServerError {
	return &StatusCollectionInternalServerError{}
}

/*
StatusCollectionInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StatusCollectionInternalServerError struct {
}

// IsSuccess returns true when this status collection internal server error response has a 2xx status code
func (o *StatusCollectionInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this status collection internal server error response has a 3xx status code
func (o *StatusCollectionInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this status collection internal server error response has a 4xx status code
func (o *StatusCollectionInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this status collection internal server error response has a 5xx status code
func (o *StatusCollectionInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this status collection internal server error response a status code equal to that given
func (o *StatusCollectionInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the status collection internal server error response
func (o *StatusCollectionInternalServerError) Code() int {
	return 500
}

func (o *StatusCollectionInternalServerError) Error() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionInternalServerError ", 500)
}

func (o *StatusCollectionInternalServerError) String() string {
	return fmt.Sprintf("[GET /status-collection][%d] statusCollectionInternalServerError ", 500)
}

func (o *StatusCollectionInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
