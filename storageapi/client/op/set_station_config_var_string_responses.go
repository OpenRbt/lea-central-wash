// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetStationConfigVarStringReader is a Reader for the SetStationConfigVarString structure.
type SetStationConfigVarStringReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetStationConfigVarStringReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetStationConfigVarStringNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewSetStationConfigVarStringInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSetStationConfigVarStringNoContent creates a SetStationConfigVarStringNoContent with default headers values
func NewSetStationConfigVarStringNoContent() *SetStationConfigVarStringNoContent {
	return &SetStationConfigVarStringNoContent{}
}

/*
SetStationConfigVarStringNoContent describes a response with status code 204, with default header values.

OK
*/
type SetStationConfigVarStringNoContent struct {
}

// IsSuccess returns true when this set station config var string no content response has a 2xx status code
func (o *SetStationConfigVarStringNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set station config var string no content response has a 3xx status code
func (o *SetStationConfigVarStringNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station config var string no content response has a 4xx status code
func (o *SetStationConfigVarStringNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station config var string no content response has a 5xx status code
func (o *SetStationConfigVarStringNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set station config var string no content response a status code equal to that given
func (o *SetStationConfigVarStringNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *SetStationConfigVarStringNoContent) Error() string {
	return fmt.Sprintf("[POST /set-station-config-var-string][%d] setStationConfigVarStringNoContent ", 204)
}

func (o *SetStationConfigVarStringNoContent) String() string {
	return fmt.Sprintf("[POST /set-station-config-var-string][%d] setStationConfigVarStringNoContent ", 204)
}

func (o *SetStationConfigVarStringNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationConfigVarStringInternalServerError creates a SetStationConfigVarStringInternalServerError with default headers values
func NewSetStationConfigVarStringInternalServerError() *SetStationConfigVarStringInternalServerError {
	return &SetStationConfigVarStringInternalServerError{}
}

/*
SetStationConfigVarStringInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type SetStationConfigVarStringInternalServerError struct {
}

// IsSuccess returns true when this set station config var string internal server error response has a 2xx status code
func (o *SetStationConfigVarStringInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station config var string internal server error response has a 3xx status code
func (o *SetStationConfigVarStringInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station config var string internal server error response has a 4xx status code
func (o *SetStationConfigVarStringInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station config var string internal server error response has a 5xx status code
func (o *SetStationConfigVarStringInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set station config var string internal server error response a status code equal to that given
func (o *SetStationConfigVarStringInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *SetStationConfigVarStringInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-station-config-var-string][%d] setStationConfigVarStringInternalServerError ", 500)
}

func (o *SetStationConfigVarStringInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-station-config-var-string][%d] setStationConfigVarStringInternalServerError ", 500)
}

func (o *SetStationConfigVarStringInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
