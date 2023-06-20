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

// KasseReader is a Reader for the Kasse structure.
type KasseReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *KasseReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewKasseOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewKasseNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewKasseInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /kasse] kasse", response, response.Code())
	}
}

// NewKasseOK creates a KasseOK with default headers values
func NewKasseOK() *KasseOK {
	return &KasseOK{}
}

/* KasseOK describes a response with status code 200, with default header values.

OK
*/
type KasseOK struct {
	Payload *model.KasseConfig
}

// IsSuccess returns true when this kasse o k response has a 2xx status code
func (o *KasseOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this kasse o k response has a 3xx status code
func (o *KasseOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this kasse o k response has a 4xx status code
func (o *KasseOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this kasse o k response has a 5xx status code
func (o *KasseOK) IsServerError() bool {
	return false
}

// IsCode returns true when this kasse o k response a status code equal to that given
func (o *KasseOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the kasse o k response
func (o *KasseOK) Code() int {
	return 200
}

func (o *KasseOK) Error() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseOK  %+v", 200, o.Payload)
}

func (o *KasseOK) String() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseOK  %+v", 200, o.Payload)
}

func (o *KasseOK) GetPayload() *model.KasseConfig {
	return o.Payload
}

func (o *KasseOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.KasseConfig)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewKasseNotFound creates a KasseNotFound with default headers values
func NewKasseNotFound() *KasseNotFound {
	return &KasseNotFound{}
}

/* KasseNotFound describes a response with status code 404, with default header values.

not found
*/
type KasseNotFound struct {
}

// IsSuccess returns true when this kasse not found response has a 2xx status code
func (o *KasseNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this kasse not found response has a 3xx status code
func (o *KasseNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this kasse not found response has a 4xx status code
func (o *KasseNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this kasse not found response has a 5xx status code
func (o *KasseNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this kasse not found response a status code equal to that given
func (o *KasseNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the kasse not found response
func (o *KasseNotFound) Code() int {
	return 404
}

func (o *KasseNotFound) Error() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseNotFound ", 404)
}

func (o *KasseNotFound) String() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseNotFound ", 404)
}

func (o *KasseNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewKasseInternalServerError creates a KasseInternalServerError with default headers values
func NewKasseInternalServerError() *KasseInternalServerError {
	return &KasseInternalServerError{}
}

/* KasseInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type KasseInternalServerError struct {
}

// IsSuccess returns true when this kasse internal server error response has a 2xx status code
func (o *KasseInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this kasse internal server error response has a 3xx status code
func (o *KasseInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this kasse internal server error response has a 4xx status code
func (o *KasseInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this kasse internal server error response has a 5xx status code
func (o *KasseInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this kasse internal server error response a status code equal to that given
func (o *KasseInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the kasse internal server error response
func (o *KasseInternalServerError) Code() int {
	return 500
}

func (o *KasseInternalServerError) Error() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseInternalServerError ", 500)
}

func (o *KasseInternalServerError) String() string {
	return fmt.Sprintf("[POST /kasse][%d] kasseInternalServerError ", 500)
}

func (o *KasseInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
