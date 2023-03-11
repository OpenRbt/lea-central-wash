// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// ProgramStopReader is a Reader for the ProgramStop structure.
type ProgramStopReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *ProgramStopReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewProgramStopNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewProgramStopNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewProgramStopInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewProgramStopNoContent creates a ProgramStopNoContent with default headers values
func NewProgramStopNoContent() *ProgramStopNoContent {
	return &ProgramStopNoContent{}
}

/*
ProgramStopNoContent describes a response with status code 204, with default header values.

OK
*/
type ProgramStopNoContent struct {
}

// IsSuccess returns true when this program stop no content response has a 2xx status code
func (o *ProgramStopNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this program stop no content response has a 3xx status code
func (o *ProgramStopNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this program stop no content response has a 4xx status code
func (o *ProgramStopNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this program stop no content response has a 5xx status code
func (o *ProgramStopNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this program stop no content response a status code equal to that given
func (o *ProgramStopNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the program stop no content response
func (o *ProgramStopNoContent) Code() int {
	return 204
}

func (o *ProgramStopNoContent) Error() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopNoContent ", 204)
}

func (o *ProgramStopNoContent) String() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopNoContent ", 204)
}

func (o *ProgramStopNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewProgramStopNotFound creates a ProgramStopNotFound with default headers values
func NewProgramStopNotFound() *ProgramStopNotFound {
	return &ProgramStopNotFound{}
}

/*
ProgramStopNotFound describes a response with status code 404, with default header values.

not found
*/
type ProgramStopNotFound struct {
	Payload string
}

// IsSuccess returns true when this program stop not found response has a 2xx status code
func (o *ProgramStopNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this program stop not found response has a 3xx status code
func (o *ProgramStopNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this program stop not found response has a 4xx status code
func (o *ProgramStopNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this program stop not found response has a 5xx status code
func (o *ProgramStopNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this program stop not found response a status code equal to that given
func (o *ProgramStopNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the program stop not found response
func (o *ProgramStopNotFound) Code() int {
	return 404
}

func (o *ProgramStopNotFound) Error() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopNotFound  %+v", 404, o.Payload)
}

func (o *ProgramStopNotFound) String() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopNotFound  %+v", 404, o.Payload)
}

func (o *ProgramStopNotFound) GetPayload() string {
	return o.Payload
}

func (o *ProgramStopNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewProgramStopInternalServerError creates a ProgramStopInternalServerError with default headers values
func NewProgramStopInternalServerError() *ProgramStopInternalServerError {
	return &ProgramStopInternalServerError{}
}

/*
ProgramStopInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type ProgramStopInternalServerError struct {
}

// IsSuccess returns true when this program stop internal server error response has a 2xx status code
func (o *ProgramStopInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this program stop internal server error response has a 3xx status code
func (o *ProgramStopInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this program stop internal server error response has a 4xx status code
func (o *ProgramStopInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this program stop internal server error response has a 5xx status code
func (o *ProgramStopInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this program stop internal server error response a status code equal to that given
func (o *ProgramStopInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the program stop internal server error response
func (o *ProgramStopInternalServerError) Code() int {
	return 500
}

func (o *ProgramStopInternalServerError) Error() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopInternalServerError ", 500)
}

func (o *ProgramStopInternalServerError) String() string {
	return fmt.Sprintf("[POST /stop-program][%d] programStopInternalServerError ", 500)
}

func (o *ProgramStopInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
ProgramStopBody ArgProgramPause
swagger:model ProgramStopBody
*/
type ProgramStopBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *ProgramStopBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	return nil
}

// Validate validates this program stop body
func (o *ProgramStopBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *ProgramStopBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if o.Hash != nil {
		if err := o.Hash.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this program stop body based on the context it is used
func (o *ProgramStopBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *ProgramStopBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {
		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *ProgramStopBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *ProgramStopBody) UnmarshalBinary(b []byte) error {
	var res ProgramStopBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
