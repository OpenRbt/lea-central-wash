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

// Run2ProgramReader is a Reader for the Run2Program structure.
type Run2ProgramReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *Run2ProgramReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewRun2ProgramNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewRun2ProgramNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewRun2ProgramInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewRun2ProgramNoContent creates a Run2ProgramNoContent with default headers values
func NewRun2ProgramNoContent() *Run2ProgramNoContent {
	return &Run2ProgramNoContent{}
}

/*
Run2ProgramNoContent describes a response with status code 204, with default header values.

OK
*/
type Run2ProgramNoContent struct {
}

// IsSuccess returns true when this run2 program no content response has a 2xx status code
func (o *Run2ProgramNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this run2 program no content response has a 3xx status code
func (o *Run2ProgramNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this run2 program no content response has a 4xx status code
func (o *Run2ProgramNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this run2 program no content response has a 5xx status code
func (o *Run2ProgramNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this run2 program no content response a status code equal to that given
func (o *Run2ProgramNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the run2 program no content response
func (o *Run2ProgramNoContent) Code() int {
	return 204
}

func (o *Run2ProgramNoContent) Error() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramNoContent ", 204)
}

func (o *Run2ProgramNoContent) String() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramNoContent ", 204)
}

func (o *Run2ProgramNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewRun2ProgramNotFound creates a Run2ProgramNotFound with default headers values
func NewRun2ProgramNotFound() *Run2ProgramNotFound {
	return &Run2ProgramNotFound{}
}

/*
Run2ProgramNotFound describes a response with status code 404, with default header values.

not found
*/
type Run2ProgramNotFound struct {
	Payload string
}

// IsSuccess returns true when this run2 program not found response has a 2xx status code
func (o *Run2ProgramNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this run2 program not found response has a 3xx status code
func (o *Run2ProgramNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this run2 program not found response has a 4xx status code
func (o *Run2ProgramNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this run2 program not found response has a 5xx status code
func (o *Run2ProgramNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this run2 program not found response a status code equal to that given
func (o *Run2ProgramNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the run2 program not found response
func (o *Run2ProgramNotFound) Code() int {
	return 404
}

func (o *Run2ProgramNotFound) Error() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramNotFound  %+v", 404, o.Payload)
}

func (o *Run2ProgramNotFound) String() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramNotFound  %+v", 404, o.Payload)
}

func (o *Run2ProgramNotFound) GetPayload() string {
	return o.Payload
}

func (o *Run2ProgramNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewRun2ProgramInternalServerError creates a Run2ProgramInternalServerError with default headers values
func NewRun2ProgramInternalServerError() *Run2ProgramInternalServerError {
	return &Run2ProgramInternalServerError{}
}

/*
Run2ProgramInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type Run2ProgramInternalServerError struct {
}

// IsSuccess returns true when this run2 program internal server error response has a 2xx status code
func (o *Run2ProgramInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this run2 program internal server error response has a 3xx status code
func (o *Run2ProgramInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this run2 program internal server error response has a 4xx status code
func (o *Run2ProgramInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this run2 program internal server error response has a 5xx status code
func (o *Run2ProgramInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this run2 program internal server error response a status code equal to that given
func (o *Run2ProgramInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the run2 program internal server error response
func (o *Run2ProgramInternalServerError) Code() int {
	return 500
}

func (o *Run2ProgramInternalServerError) Error() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramInternalServerError ", 500)
}

func (o *Run2ProgramInternalServerError) String() string {
	return fmt.Sprintf("[POST /run-2program][%d] run2ProgramInternalServerError ", 500)
}

func (o *Run2ProgramInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
Run2ProgramBody ArgRun2Program
swagger:model Run2ProgramBody
*/
type Run2ProgramBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// preflight
	// Required: true
	Preflight *bool `json:"preflight"`

	// program ID
	// Required: true
	ProgramID *int64 `json:"programID"`

	// program ID 2
	// Required: true
	ProgramID2 *int64 `json:"programID2"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *Run2ProgramBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// preflight
		// Required: true
		Preflight *bool `json:"preflight"`

		// program ID
		// Required: true
		ProgramID *int64 `json:"programID"`

		// program ID 2
		// Required: true
		ProgramID2 *int64 `json:"programID2"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Preflight = props.Preflight
	o.ProgramID = props.ProgramID
	o.ProgramID2 = props.ProgramID2
	return nil
}

// Validate validates this run2 program body
func (o *Run2ProgramBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validatePreflight(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateProgramID(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateProgramID2(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *Run2ProgramBody) validateHash(formats strfmt.Registry) error {

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

func (o *Run2ProgramBody) validatePreflight(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"preflight", "body", o.Preflight); err != nil {
		return err
	}

	return nil
}

func (o *Run2ProgramBody) validateProgramID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"programID", "body", o.ProgramID); err != nil {
		return err
	}

	return nil
}

func (o *Run2ProgramBody) validateProgramID2(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"programID2", "body", o.ProgramID2); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this run2 program body based on the context it is used
func (o *Run2ProgramBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *Run2ProgramBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *Run2ProgramBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *Run2ProgramBody) UnmarshalBinary(b []byte) error {
	var res Run2ProgramBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
