// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetProgramReader is a Reader for the SetProgram structure.
type SetProgramReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetProgramReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetProgramNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewSetProgramInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSetProgramNoContent creates a SetProgramNoContent with default headers values
func NewSetProgramNoContent() *SetProgramNoContent {
	return &SetProgramNoContent{}
}

/* SetProgramNoContent describes a response with status code 204, with default header values.

OK
*/
type SetProgramNoContent struct {
}

// IsSuccess returns true when this set program no content response has a 2xx status code
func (o *SetProgramNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set program no content response has a 3xx status code
func (o *SetProgramNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set program no content response has a 4xx status code
func (o *SetProgramNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set program no content response has a 5xx status code
func (o *SetProgramNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set program no content response a status code equal to that given
func (o *SetProgramNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *SetProgramNoContent) Error() string {
	return fmt.Sprintf("[POST /set-program][%d] setProgramNoContent ", 204)
}

func (o *SetProgramNoContent) String() string {
	return fmt.Sprintf("[POST /set-program][%d] setProgramNoContent ", 204)
}

func (o *SetProgramNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetProgramInternalServerError creates a SetProgramInternalServerError with default headers values
func NewSetProgramInternalServerError() *SetProgramInternalServerError {
	return &SetProgramInternalServerError{}
}

/* SetProgramInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SetProgramInternalServerError struct {
}

// IsSuccess returns true when this set program internal server error response has a 2xx status code
func (o *SetProgramInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set program internal server error response has a 3xx status code
func (o *SetProgramInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set program internal server error response has a 4xx status code
func (o *SetProgramInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set program internal server error response has a 5xx status code
func (o *SetProgramInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set program internal server error response a status code equal to that given
func (o *SetProgramInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *SetProgramInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-program][%d] setProgramInternalServerError ", 500)
}

func (o *SetProgramInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-program][%d] setProgramInternalServerError ", 500)
}

func (o *SetProgramInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
