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

// ProgramsReader is a Reader for the Programs structure.
type ProgramsReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *ProgramsReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewProgramsOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewProgramsInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewProgramsOK creates a ProgramsOK with default headers values
func NewProgramsOK() *ProgramsOK {
	return &ProgramsOK{}
}

/*
ProgramsOK describes a response with status code 200, with default header values.

OK
*/
type ProgramsOK struct {
	Payload []*model.Program
}

// IsSuccess returns true when this programs o k response has a 2xx status code
func (o *ProgramsOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this programs o k response has a 3xx status code
func (o *ProgramsOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this programs o k response has a 4xx status code
func (o *ProgramsOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this programs o k response has a 5xx status code
func (o *ProgramsOK) IsServerError() bool {
	return false
}

// IsCode returns true when this programs o k response a status code equal to that given
func (o *ProgramsOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the programs o k response
func (o *ProgramsOK) Code() int {
	return 200
}

func (o *ProgramsOK) Error() string {
	return fmt.Sprintf("[POST /programs][%d] programsOK  %+v", 200, o.Payload)
}

func (o *ProgramsOK) String() string {
	return fmt.Sprintf("[POST /programs][%d] programsOK  %+v", 200, o.Payload)
}

func (o *ProgramsOK) GetPayload() []*model.Program {
	return o.Payload
}

func (o *ProgramsOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewProgramsInternalServerError creates a ProgramsInternalServerError with default headers values
func NewProgramsInternalServerError() *ProgramsInternalServerError {
	return &ProgramsInternalServerError{}
}

/*
ProgramsInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type ProgramsInternalServerError struct {
}

// IsSuccess returns true when this programs internal server error response has a 2xx status code
func (o *ProgramsInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this programs internal server error response has a 3xx status code
func (o *ProgramsInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this programs internal server error response has a 4xx status code
func (o *ProgramsInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this programs internal server error response has a 5xx status code
func (o *ProgramsInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this programs internal server error response a status code equal to that given
func (o *ProgramsInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the programs internal server error response
func (o *ProgramsInternalServerError) Code() int {
	return 500
}

func (o *ProgramsInternalServerError) Error() string {
	return fmt.Sprintf("[POST /programs][%d] programsInternalServerError ", 500)
}

func (o *ProgramsInternalServerError) String() string {
	return fmt.Sprintf("[POST /programs][%d] programsInternalServerError ", 500)
}

func (o *ProgramsInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
ProgramsBody ArgPrograms
swagger:model ProgramsBody
*/
type ProgramsBody struct {

	// program ID
	// Minimum: 1
	ProgramID *int64 `json:"programID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *ProgramsBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// program ID
		// Minimum: 1
		ProgramID *int64 `json:"programID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ProgramID = props.ProgramID
	return nil
}

// Validate validates this programs body
func (o *ProgramsBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateProgramID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *ProgramsBody) validateProgramID(formats strfmt.Registry) error {
	if swag.IsZero(o.ProgramID) { // not required
		return nil
	}

	if err := validate.MinimumInt("args"+"."+"programID", "body", *o.ProgramID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this programs body based on context it is used
func (o *ProgramsBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *ProgramsBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *ProgramsBody) UnmarshalBinary(b []byte) error {
	var res ProgramsBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
