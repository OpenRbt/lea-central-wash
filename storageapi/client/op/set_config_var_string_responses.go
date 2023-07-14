// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetConfigVarStringReader is a Reader for the SetConfigVarString structure.
type SetConfigVarStringReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetConfigVarStringReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetConfigVarStringNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewSetConfigVarStringInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /set-config-var-string] setConfigVarString", response, response.Code())
	}
}

// NewSetConfigVarStringNoContent creates a SetConfigVarStringNoContent with default headers values
func NewSetConfigVarStringNoContent() *SetConfigVarStringNoContent {
	return &SetConfigVarStringNoContent{}
}

/*
SetConfigVarStringNoContent describes a response with status code 204, with default header values.

OK
*/
type SetConfigVarStringNoContent struct {
}

// IsSuccess returns true when this set config var string no content response has a 2xx status code
func (o *SetConfigVarStringNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set config var string no content response has a 3xx status code
func (o *SetConfigVarStringNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set config var string no content response has a 4xx status code
func (o *SetConfigVarStringNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set config var string no content response has a 5xx status code
func (o *SetConfigVarStringNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set config var string no content response a status code equal to that given
func (o *SetConfigVarStringNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the set config var string no content response
func (o *SetConfigVarStringNoContent) Code() int {
	return 204
}

func (o *SetConfigVarStringNoContent) Error() string {
	return fmt.Sprintf("[POST /set-config-var-string][%d] setConfigVarStringNoContent ", 204)
}

func (o *SetConfigVarStringNoContent) String() string {
	return fmt.Sprintf("[POST /set-config-var-string][%d] setConfigVarStringNoContent ", 204)
}

func (o *SetConfigVarStringNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetConfigVarStringInternalServerError creates a SetConfigVarStringInternalServerError with default headers values
func NewSetConfigVarStringInternalServerError() *SetConfigVarStringInternalServerError {
	return &SetConfigVarStringInternalServerError{}
}

/*
SetConfigVarStringInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type SetConfigVarStringInternalServerError struct {
}

// IsSuccess returns true when this set config var string internal server error response has a 2xx status code
func (o *SetConfigVarStringInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set config var string internal server error response has a 3xx status code
func (o *SetConfigVarStringInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set config var string internal server error response has a 4xx status code
func (o *SetConfigVarStringInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set config var string internal server error response has a 5xx status code
func (o *SetConfigVarStringInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set config var string internal server error response a status code equal to that given
func (o *SetConfigVarStringInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the set config var string internal server error response
func (o *SetConfigVarStringInternalServerError) Code() int {
	return 500
}

func (o *SetConfigVarStringInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-config-var-string][%d] setConfigVarStringInternalServerError ", 500)
}

func (o *SetConfigVarStringInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-config-var-string][%d] setConfigVarStringInternalServerError ", 500)
}

func (o *SetConfigVarStringInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
