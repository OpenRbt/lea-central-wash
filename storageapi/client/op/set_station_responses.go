// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"

	strfmt "github.com/go-openapi/strfmt"
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
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewSetStationNoContent creates a SetStationNoContent with default headers values
func NewSetStationNoContent() *SetStationNoContent {
	return &SetStationNoContent{}
}

/*SetStationNoContent handles this case with default header values.

OK
*/
type SetStationNoContent struct {
}

func (o *SetStationNoContent) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNoContent ", 204)
}

func (o *SetStationNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationUnauthorized creates a SetStationUnauthorized with default headers values
func NewSetStationUnauthorized() *SetStationUnauthorized {
	return &SetStationUnauthorized{}
}

/*SetStationUnauthorized handles this case with default header values.

Access denied. It will happen when you try to change the ID at the station online.
*/
type SetStationUnauthorized struct {
}

func (o *SetStationUnauthorized) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnauthorized ", 401)
}

func (o *SetStationUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationNotFound creates a SetStationNotFound with default headers values
func NewSetStationNotFound() *SetStationNotFound {
	return &SetStationNotFound{}
}

/*SetStationNotFound handles this case with default header values.

not found
*/
type SetStationNotFound struct {
}

func (o *SetStationNotFound) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationNotFound ", 404)
}

func (o *SetStationNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationUnprocessableEntity creates a SetStationUnprocessableEntity with default headers values
func NewSetStationUnprocessableEntity() *SetStationUnprocessableEntity {
	return &SetStationUnprocessableEntity{}
}

/*SetStationUnprocessableEntity handles this case with default header values.

validation error
*/
type SetStationUnprocessableEntity struct {
}

func (o *SetStationUnprocessableEntity) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationUnprocessableEntity ", 422)
}

func (o *SetStationUnprocessableEntity) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationInternalServerError creates a SetStationInternalServerError with default headers values
func NewSetStationInternalServerError() *SetStationInternalServerError {
	return &SetStationInternalServerError{}
}

/*SetStationInternalServerError handles this case with default header values.

internal error
*/
type SetStationInternalServerError struct {
}

func (o *SetStationInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-station][%d] setStationInternalServerError ", 500)
}

func (o *SetStationInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
