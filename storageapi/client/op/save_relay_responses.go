// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"

	strfmt "github.com/go-openapi/strfmt"
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
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewSaveRelayNoContent creates a SaveRelayNoContent with default headers values
func NewSaveRelayNoContent() *SaveRelayNoContent {
	return &SaveRelayNoContent{}
}

/*SaveRelayNoContent handles this case with default header values.

OK
*/
type SaveRelayNoContent struct {
}

func (o *SaveRelayNoContent) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNoContent ", 204)
}

func (o *SaveRelayNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveRelayNotFound creates a SaveRelayNotFound with default headers values
func NewSaveRelayNotFound() *SaveRelayNotFound {
	return &SaveRelayNotFound{}
}

/*SaveRelayNotFound handles this case with default header values.

not found
*/
type SaveRelayNotFound struct {
}

func (o *SaveRelayNotFound) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayNotFound ", 404)
}

func (o *SaveRelayNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveRelayInternalServerError creates a SaveRelayInternalServerError with default headers values
func NewSaveRelayInternalServerError() *SaveRelayInternalServerError {
	return &SaveRelayInternalServerError{}
}

/*SaveRelayInternalServerError handles this case with default header values.

internal error
*/
type SaveRelayInternalServerError struct {
}

func (o *SaveRelayInternalServerError) Error() string {
	return fmt.Sprintf("[POST /save-relay][%d] saveRelayInternalServerError ", 500)
}

func (o *SaveRelayInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
