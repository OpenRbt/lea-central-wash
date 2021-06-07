// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// SetCardReaderConfigReader is a Reader for the SetCardReaderConfig structure.
type SetCardReaderConfigReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetCardReaderConfigReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSetCardReaderConfigNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewSetCardReaderConfigNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 422:
		result := NewSetCardReaderConfigUnprocessableEntity()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSetCardReaderConfigInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSetCardReaderConfigNoContent creates a SetCardReaderConfigNoContent with default headers values
func NewSetCardReaderConfigNoContent() *SetCardReaderConfigNoContent {
	return &SetCardReaderConfigNoContent{}
}

/* SetCardReaderConfigNoContent describes a response with status code 204, with default header values.

OK
*/
type SetCardReaderConfigNoContent struct {
}

func (o *SetCardReaderConfigNoContent) Error() string {
	return fmt.Sprintf("[POST /set-card-reader-config][%d] setCardReaderConfigNoContent ", 204)
}

func (o *SetCardReaderConfigNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetCardReaderConfigNotFound creates a SetCardReaderConfigNotFound with default headers values
func NewSetCardReaderConfigNotFound() *SetCardReaderConfigNotFound {
	return &SetCardReaderConfigNotFound{}
}

/* SetCardReaderConfigNotFound describes a response with status code 404, with default header values.

not found
*/
type SetCardReaderConfigNotFound struct {
}

func (o *SetCardReaderConfigNotFound) Error() string {
	return fmt.Sprintf("[POST /set-card-reader-config][%d] setCardReaderConfigNotFound ", 404)
}

func (o *SetCardReaderConfigNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetCardReaderConfigUnprocessableEntity creates a SetCardReaderConfigUnprocessableEntity with default headers values
func NewSetCardReaderConfigUnprocessableEntity() *SetCardReaderConfigUnprocessableEntity {
	return &SetCardReaderConfigUnprocessableEntity{}
}

/* SetCardReaderConfigUnprocessableEntity describes a response with status code 422, with default header values.

validation error
*/
type SetCardReaderConfigUnprocessableEntity struct {
	Payload string
}

func (o *SetCardReaderConfigUnprocessableEntity) Error() string {
	return fmt.Sprintf("[POST /set-card-reader-config][%d] setCardReaderConfigUnprocessableEntity  %+v", 422, o.Payload)
}
func (o *SetCardReaderConfigUnprocessableEntity) GetPayload() string {
	return o.Payload
}

func (o *SetCardReaderConfigUnprocessableEntity) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewSetCardReaderConfigInternalServerError creates a SetCardReaderConfigInternalServerError with default headers values
func NewSetCardReaderConfigInternalServerError() *SetCardReaderConfigInternalServerError {
	return &SetCardReaderConfigInternalServerError{}
}

/* SetCardReaderConfigInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SetCardReaderConfigInternalServerError struct {
}

func (o *SetCardReaderConfigInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-card-reader-config][%d] setCardReaderConfigInternalServerError ", 500)
}

func (o *SetCardReaderConfigInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
