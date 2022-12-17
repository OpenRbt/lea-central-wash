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

// DispenserStopReader is a Reader for the DispenserStop structure.
type DispenserStopReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *DispenserStopReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewDispenserStopNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewDispenserStopNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewDispenserStopInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewDispenserStopNoContent creates a DispenserStopNoContent with default headers values
func NewDispenserStopNoContent() *DispenserStopNoContent {
	return &DispenserStopNoContent{}
}

/*
DispenserStopNoContent describes a response with status code 204, with default header values.

OK
*/
type DispenserStopNoContent struct {
}

// IsSuccess returns true when this dispenser stop no content response has a 2xx status code
func (o *DispenserStopNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this dispenser stop no content response has a 3xx status code
func (o *DispenserStopNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this dispenser stop no content response has a 4xx status code
func (o *DispenserStopNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this dispenser stop no content response has a 5xx status code
func (o *DispenserStopNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this dispenser stop no content response a status code equal to that given
func (o *DispenserStopNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the dispenser stop no content response
func (o *DispenserStopNoContent) Code() int {
	return 204
}

func (o *DispenserStopNoContent) Error() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopNoContent ", 204)
}

func (o *DispenserStopNoContent) String() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopNoContent ", 204)
}

func (o *DispenserStopNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDispenserStopNotFound creates a DispenserStopNotFound with default headers values
func NewDispenserStopNotFound() *DispenserStopNotFound {
	return &DispenserStopNotFound{}
}

/*
DispenserStopNotFound describes a response with status code 404, with default header values.

not found
*/
type DispenserStopNotFound struct {
	Payload string
}

// IsSuccess returns true when this dispenser stop not found response has a 2xx status code
func (o *DispenserStopNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this dispenser stop not found response has a 3xx status code
func (o *DispenserStopNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this dispenser stop not found response has a 4xx status code
func (o *DispenserStopNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this dispenser stop not found response has a 5xx status code
func (o *DispenserStopNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this dispenser stop not found response a status code equal to that given
func (o *DispenserStopNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the dispenser stop not found response
func (o *DispenserStopNotFound) Code() int {
	return 404
}

func (o *DispenserStopNotFound) Error() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopNotFound  %+v", 404, o.Payload)
}

func (o *DispenserStopNotFound) String() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopNotFound  %+v", 404, o.Payload)
}

func (o *DispenserStopNotFound) GetPayload() string {
	return o.Payload
}

func (o *DispenserStopNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewDispenserStopInternalServerError creates a DispenserStopInternalServerError with default headers values
func NewDispenserStopInternalServerError() *DispenserStopInternalServerError {
	return &DispenserStopInternalServerError{}
}

/*
DispenserStopInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type DispenserStopInternalServerError struct {
}

// IsSuccess returns true when this dispenser stop internal server error response has a 2xx status code
func (o *DispenserStopInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this dispenser stop internal server error response has a 3xx status code
func (o *DispenserStopInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this dispenser stop internal server error response has a 4xx status code
func (o *DispenserStopInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this dispenser stop internal server error response has a 5xx status code
func (o *DispenserStopInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this dispenser stop internal server error response a status code equal to that given
func (o *DispenserStopInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the dispenser stop internal server error response
func (o *DispenserStopInternalServerError) Code() int {
	return 500
}

func (o *DispenserStopInternalServerError) Error() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopInternalServerError ", 500)
}

func (o *DispenserStopInternalServerError) String() string {
	return fmt.Sprintf("[POST /stop-dispenser][%d] dispenserStopInternalServerError ", 500)
}

func (o *DispenserStopInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
DispenserStopBody ArgDispenserStop
swagger:model DispenserStopBody
*/
type DispenserStopBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// stop program ID
	// Required: true
	StopProgramID *int64 `json:"stopProgramID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DispenserStopBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// stop program ID
		// Required: true
		StopProgramID *int64 `json:"stopProgramID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.StopProgramID = props.StopProgramID
	return nil
}

// Validate validates this dispenser stop body
func (o *DispenserStopBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStopProgramID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DispenserStopBody) validateHash(formats strfmt.Registry) error {

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

func (o *DispenserStopBody) validateStopProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stopProgramID", "body", o.StopProgramID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this dispenser stop body based on the context it is used
func (o *DispenserStopBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DispenserStopBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *DispenserStopBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DispenserStopBody) UnmarshalBinary(b []byte) error {
	var res DispenserStopBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}