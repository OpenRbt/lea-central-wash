// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SaveMoneyReader is a Reader for the SaveMoney structure.
type SaveMoneyReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SaveMoneyReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSaveMoneyNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewSaveMoneyNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSaveMoneyInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSaveMoneyNoContent creates a SaveMoneyNoContent with default headers values
func NewSaveMoneyNoContent() *SaveMoneyNoContent {
	return &SaveMoneyNoContent{}
}

/* SaveMoneyNoContent describes a response with status code 204, with default header values.

OK
*/
type SaveMoneyNoContent struct {
}

// IsSuccess returns true when this save money no content response has a 2xx status code
func (o *SaveMoneyNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this save money no content response has a 3xx status code
func (o *SaveMoneyNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save money no content response has a 4xx status code
func (o *SaveMoneyNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this save money no content response has a 5xx status code
func (o *SaveMoneyNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this save money no content response a status code equal to that given
func (o *SaveMoneyNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *SaveMoneyNoContent) Error() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyNoContent ", 204)
}

func (o *SaveMoneyNoContent) String() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyNoContent ", 204)
}

func (o *SaveMoneyNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveMoneyNotFound creates a SaveMoneyNotFound with default headers values
func NewSaveMoneyNotFound() *SaveMoneyNotFound {
	return &SaveMoneyNotFound{}
}

/* SaveMoneyNotFound describes a response with status code 404, with default header values.

not found
*/
type SaveMoneyNotFound struct {
}

// IsSuccess returns true when this save money not found response has a 2xx status code
func (o *SaveMoneyNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save money not found response has a 3xx status code
func (o *SaveMoneyNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save money not found response has a 4xx status code
func (o *SaveMoneyNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this save money not found response has a 5xx status code
func (o *SaveMoneyNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this save money not found response a status code equal to that given
func (o *SaveMoneyNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *SaveMoneyNotFound) Error() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyNotFound ", 404)
}

func (o *SaveMoneyNotFound) String() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyNotFound ", 404)
}

func (o *SaveMoneyNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveMoneyInternalServerError creates a SaveMoneyInternalServerError with default headers values
func NewSaveMoneyInternalServerError() *SaveMoneyInternalServerError {
	return &SaveMoneyInternalServerError{}
}

/* SaveMoneyInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SaveMoneyInternalServerError struct {
}

// IsSuccess returns true when this save money internal server error response has a 2xx status code
func (o *SaveMoneyInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save money internal server error response has a 3xx status code
func (o *SaveMoneyInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save money internal server error response has a 4xx status code
func (o *SaveMoneyInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this save money internal server error response has a 5xx status code
func (o *SaveMoneyInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this save money internal server error response a status code equal to that given
func (o *SaveMoneyInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *SaveMoneyInternalServerError) Error() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyInternalServerError ", 500)
}

func (o *SaveMoneyInternalServerError) String() string {
	return fmt.Sprintf("[POST /save-money][%d] saveMoneyInternalServerError ", 500)
}

func (o *SaveMoneyInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
