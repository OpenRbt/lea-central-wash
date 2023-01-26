// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetStationReader is a Reader for the SetStation structure.
type SetStationReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetStationReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetStationNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewSetStationUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 404:
		result := NewSetStationNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 422:
		result := NewSetStationUnprocessableEntity()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSetStationInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSetStationNoContent creates a SetStationNoContent with default headers values
func NewSetStationNoContent() *SetStationNoContent {
	return &SetStationNoContent{}
}

/*
SetStationNoContent describes a response with status code 204, with default header values.

OK
*/
type SetStationNoContent struct {
}

// IsSuccess returns true when this set station no content response has a 2xx status code
func (o *SetStationNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set station no content response has a 3xx status code
func (o *SetStationNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station no content response has a 4xx status code
func (o *SetStationNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station no content response has a 5xx status code
func (o *SetStationNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set station no content response a status code equal to that given
func (o *SetStationNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *SetStationNoContent) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNoContent ", 204)
}

func (o *SetStationNoContent) String() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNoContent ", 204)
}

func (o *SetStationNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationUnauthorized creates a SetStationUnauthorized with default headers values
func NewSetStationUnauthorized() *SetStationUnauthorized {
	return &SetStationUnauthorized{}
}

/*
SetStationUnauthorized describes a response with status code 401, with default header values.

Access denied. It will happen when you try to change the ID at the station online.
*/
type SetStationUnauthorized struct {
}

// IsSuccess returns true when this set station unauthorized response has a 2xx status code
func (o *SetStationUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station unauthorized response has a 3xx status code
func (o *SetStationUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station unauthorized response has a 4xx status code
func (o *SetStationUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this set station unauthorized response has a 5xx status code
func (o *SetStationUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this set station unauthorized response a status code equal to that given
func (o *SetStationUnauthorized) IsCode(code int) bool {
	return code == 401
}

func (o *SetStationUnauthorized) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnauthorized ", 401)
}

func (o *SetStationUnauthorized) String() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnauthorized ", 401)
}

func (o *SetStationUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationNotFound creates a SetStationNotFound with default headers values
func NewSetStationNotFound() *SetStationNotFound {
	return &SetStationNotFound{}
}

/*
SetStationNotFound describes a response with status code 404, with default header values.

not found
*/
type SetStationNotFound struct {
}

// IsSuccess returns true when this set station not found response has a 2xx status code
func (o *SetStationNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station not found response has a 3xx status code
func (o *SetStationNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station not found response has a 4xx status code
func (o *SetStationNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this set station not found response has a 5xx status code
func (o *SetStationNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this set station not found response a status code equal to that given
func (o *SetStationNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *SetStationNotFound) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNotFound ", 404)
}

func (o *SetStationNotFound) String() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNotFound ", 404)
}

func (o *SetStationNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationUnprocessableEntity creates a SetStationUnprocessableEntity with default headers values
func NewSetStationUnprocessableEntity() *SetStationUnprocessableEntity {
	return &SetStationUnprocessableEntity{}
}

/*
SetStationUnprocessableEntity describes a response with status code 422, with default header values.

validation error
*/
type SetStationUnprocessableEntity struct {
}

// IsSuccess returns true when this set station unprocessable entity response has a 2xx status code
func (o *SetStationUnprocessableEntity) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station unprocessable entity response has a 3xx status code
func (o *SetStationUnprocessableEntity) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station unprocessable entity response has a 4xx status code
func (o *SetStationUnprocessableEntity) IsClientError() bool {
	return true
}

// IsServerError returns true when this set station unprocessable entity response has a 5xx status code
func (o *SetStationUnprocessableEntity) IsServerError() bool {
	return false
}

// IsCode returns true when this set station unprocessable entity response a status code equal to that given
func (o *SetStationUnprocessableEntity) IsCode(code int) bool {
	return code == 422
}

func (o *SetStationUnprocessableEntity) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnprocessableEntity ", 422)
}

func (o *SetStationUnprocessableEntity) String() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnprocessableEntity ", 422)
}

func (o *SetStationUnprocessableEntity) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationInternalServerError creates a SetStationInternalServerError with default headers values
func NewSetStationInternalServerError() *SetStationInternalServerError {
	return &SetStationInternalServerError{}
}

/*
SetStationInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SetStationInternalServerError struct {
}

// IsSuccess returns true when this set station internal server error response has a 2xx status code
func (o *SetStationInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station internal server error response has a 3xx status code
func (o *SetStationInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station internal server error response has a 4xx status code
func (o *SetStationInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station internal server error response has a 5xx status code
func (o *SetStationInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set station internal server error response a status code equal to that given
func (o *SetStationInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *SetStationInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationInternalServerError ", 500)
}

func (o *SetStationInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationInternalServerError ", 500)
}

func (o *SetStationInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
