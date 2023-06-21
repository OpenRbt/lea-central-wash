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

// StationProgramByHashReader is a Reader for the StationProgramByHash structure.
type StationProgramByHashReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationProgramByHashReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStationProgramByHashOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewStationProgramByHashInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /station-program-by-hash] stationProgramByHash", response, response.Code())
	}
}

// NewStationProgramByHashOK creates a StationProgramByHashOK with default headers values
func NewStationProgramByHashOK() *StationProgramByHashOK {
	return &StationProgramByHashOK{}
}

/* StationProgramByHashOK describes a response with status code 200, with default header values.

OK
*/
type StationProgramByHashOK struct {
	Payload *model.StationPrograms
}

// IsSuccess returns true when this station program by hash o k response has a 2xx status code
func (o *StationProgramByHashOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station program by hash o k response has a 3xx status code
func (o *StationProgramByHashOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station program by hash o k response has a 4xx status code
func (o *StationProgramByHashOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station program by hash o k response has a 5xx status code
func (o *StationProgramByHashOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station program by hash o k response a status code equal to that given
func (o *StationProgramByHashOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the station program by hash o k response
func (o *StationProgramByHashOK) Code() int {
	return 200
}

func (o *StationProgramByHashOK) Error() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashOK  %+v", 200, o.Payload)
}

func (o *StationProgramByHashOK) String() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashOK  %+v", 200, o.Payload)
}

func (o *StationProgramByHashOK) GetPayload() *model.StationPrograms {
	return o.Payload
}

func (o *StationProgramByHashOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationPrograms)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationProgramByHashInternalServerError creates a StationProgramByHashInternalServerError with default headers values
func NewStationProgramByHashInternalServerError() *StationProgramByHashInternalServerError {
	return &StationProgramByHashInternalServerError{}
}

/* StationProgramByHashInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationProgramByHashInternalServerError struct {
}

// IsSuccess returns true when this station program by hash internal server error response has a 2xx status code
func (o *StationProgramByHashInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station program by hash internal server error response has a 3xx status code
func (o *StationProgramByHashInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station program by hash internal server error response has a 4xx status code
func (o *StationProgramByHashInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station program by hash internal server error response has a 5xx status code
func (o *StationProgramByHashInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station program by hash internal server error response a status code equal to that given
func (o *StationProgramByHashInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the station program by hash internal server error response
func (o *StationProgramByHashInternalServerError) Code() int {
	return 500
}

func (o *StationProgramByHashInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashInternalServerError ", 500)
}

func (o *StationProgramByHashInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashInternalServerError ", 500)
}

func (o *StationProgramByHashInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*StationProgramByHashBody ArgStationProgramByHash
swagger:model StationProgramByHashBody
*/
type StationProgramByHashBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationProgramByHashBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this station program by hash body
func (o *StationProgramByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationProgramByHashBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this station program by hash body based on the context it is used
func (o *StationProgramByHashBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationProgramByHashBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *StationProgramByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationProgramByHashBody) UnmarshalBinary(b []byte) error {
	var res StationProgramByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
