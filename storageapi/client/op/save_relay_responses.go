// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SaveRelayReader is a Reader for the SaveRelay structure.
type SaveRelayReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SaveRelayReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSaveRelayNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewSaveRelayNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSaveRelayInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /save-relay] saveRelay", response, response.Code())
	}
}

// NewSaveRelayNoContent creates a SaveRelayNoContent with default headers values
func NewSaveRelayNoContent() *SaveRelayNoContent {
	return &SaveRelayNoContent{}
}

/*
SaveRelayNoContent describes a response with status code 204, with default header values.

OK
*/
type SaveRelayNoContent struct {
}

// IsSuccess returns true when this save relay no content response has a 2xx status code
func (o *SaveRelayNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this save relay no content response has a 3xx status code
func (o *SaveRelayNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save relay no content response has a 4xx status code
func (o *SaveRelayNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this save relay no content response has a 5xx status code
func (o *SaveRelayNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this save relay no content response a status code equal to that given
func (o *SaveRelayNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the save relay no content response
func (o *SaveRelayNoContent) Code() int {
	return 204
}

func (o *SaveRelayNoContent) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNoContent ", 204)
}

func (o *SaveRelayNoContent) String() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNoContent ", 204)
}

func (o *SaveRelayNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveRelayNotFound creates a SaveRelayNotFound with default headers values
func NewSaveRelayNotFound() *SaveRelayNotFound {
	return &SaveRelayNotFound{}
}

/*
SaveRelayNotFound describes a response with status code 404, with default header values.

not found
*/
type SaveRelayNotFound struct {
}

// IsSuccess returns true when this save relay not found response has a 2xx status code
func (o *SaveRelayNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save relay not found response has a 3xx status code
func (o *SaveRelayNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save relay not found response has a 4xx status code
func (o *SaveRelayNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this save relay not found response has a 5xx status code
func (o *SaveRelayNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this save relay not found response a status code equal to that given
func (o *SaveRelayNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the save relay not found response
func (o *SaveRelayNotFound) Code() int {
	return 404
}

func (o *SaveRelayNotFound) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNotFound ", 404)
}

func (o *SaveRelayNotFound) String() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNotFound ", 404)
}

func (o *SaveRelayNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveRelayInternalServerError creates a SaveRelayInternalServerError with default headers values
func NewSaveRelayInternalServerError() *SaveRelayInternalServerError {
	return &SaveRelayInternalServerError{}
}

/*
SaveRelayInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SaveRelayInternalServerError struct {
}

// IsSuccess returns true when this save relay internal server error response has a 2xx status code
func (o *SaveRelayInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save relay internal server error response has a 3xx status code
func (o *SaveRelayInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save relay internal server error response has a 4xx status code
func (o *SaveRelayInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this save relay internal server error response has a 5xx status code
func (o *SaveRelayInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this save relay internal server error response a status code equal to that given
func (o *SaveRelayInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the save relay internal server error response
func (o *SaveRelayInternalServerError) Code() int {
	return 500
}

func (o *SaveRelayInternalServerError) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayInternalServerError ", 500)
}

func (o *SaveRelayInternalServerError) String() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayInternalServerError ", 500)
}

func (o *SaveRelayInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
