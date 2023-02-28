// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetConfigVarIntReader is a Reader for the SetConfigVarInt structure.
type SetConfigVarIntReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetConfigVarIntReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetConfigVarIntNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewSetConfigVarIntInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSetConfigVarIntNoContent creates a SetConfigVarIntNoContent with default headers values
func NewSetConfigVarIntNoContent() *SetConfigVarIntNoContent {
	return &SetConfigVarIntNoContent{}
}

/*
SetConfigVarIntNoContent describes a response with status code 204, with default header values.

OK
*/
type SetConfigVarIntNoContent struct {
}

// IsSuccess returns true when this set config var int no content response has a 2xx status code
func (o *SetConfigVarIntNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set config var int no content response has a 3xx status code
func (o *SetConfigVarIntNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set config var int no content response has a 4xx status code
func (o *SetConfigVarIntNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set config var int no content response has a 5xx status code
func (o *SetConfigVarIntNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set config var int no content response a status code equal to that given
func (o *SetConfigVarIntNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the set config var int no content response
func (o *SetConfigVarIntNoContent) Code() int {
	return 204
}

func (o *SetConfigVarIntNoContent) Error() string {
	return fmt.Sprintf("[POST /set-config-var-int][%d] setConfigVarIntNoContent ", 204)
}

func (o *SetConfigVarIntNoContent) String() string {
	return fmt.Sprintf("[POST /set-config-var-int][%d] setConfigVarIntNoContent ", 204)
}

func (o *SetConfigVarIntNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetConfigVarIntInternalServerError creates a SetConfigVarIntInternalServerError with default headers values
func NewSetConfigVarIntInternalServerError() *SetConfigVarIntInternalServerError {
	return &SetConfigVarIntInternalServerError{}
}

/*
SetConfigVarIntInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type SetConfigVarIntInternalServerError struct {
}

// IsSuccess returns true when this set config var int internal server error response has a 2xx status code
func (o *SetConfigVarIntInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set config var int internal server error response has a 3xx status code
func (o *SetConfigVarIntInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set config var int internal server error response has a 4xx status code
func (o *SetConfigVarIntInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set config var int internal server error response has a 5xx status code
func (o *SetConfigVarIntInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set config var int internal server error response a status code equal to that given
func (o *SetConfigVarIntInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the set config var int internal server error response
func (o *SetConfigVarIntInternalServerError) Code() int {
	return 500
}

func (o *SetConfigVarIntInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-config-var-int][%d] setConfigVarIntInternalServerError ", 500)
}

func (o *SetConfigVarIntInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-config-var-int][%d] setConfigVarIntInternalServerError ", 500)
}

func (o *SetConfigVarIntInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
