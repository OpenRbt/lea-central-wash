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

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// StationByHashReader is a Reader for the StationByHash structure.
type StationByHashReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationByHashReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStationByHashOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewStationByHashInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /station-by-hash] stationByHash", response, response.Code())
	}
}

// NewStationByHashOK creates a StationByHashOK with default headers values
func NewStationByHashOK() *StationByHashOK {
	return &StationByHashOK{}
}

/*
StationByHashOK describes a response with status code 200, with default header values.

OK
*/
type StationByHashOK struct {
	Payload int64
}

// IsSuccess returns true when this station by hash o k response has a 2xx status code
func (o *StationByHashOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station by hash o k response has a 3xx status code
func (o *StationByHashOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station by hash o k response has a 4xx status code
func (o *StationByHashOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station by hash o k response has a 5xx status code
func (o *StationByHashOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station by hash o k response a status code equal to that given
func (o *StationByHashOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the station by hash o k response
func (o *StationByHashOK) Code() int {
	return 200
}

func (o *StationByHashOK) Error() string {
	return fmt.Sprintf("[POST /station-by-hash][%d] stationByHashOK  %+v", 200, o.Payload)
}

func (o *StationByHashOK) String() string {
	return fmt.Sprintf("[POST /station-by-hash][%d] stationByHashOK  %+v", 200, o.Payload)
}

func (o *StationByHashOK) GetPayload() int64 {
	return o.Payload
}

func (o *StationByHashOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationByHashInternalServerError creates a StationByHashInternalServerError with default headers values
func NewStationByHashInternalServerError() *StationByHashInternalServerError {
	return &StationByHashInternalServerError{}
}

/*
StationByHashInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationByHashInternalServerError struct {
}

// IsSuccess returns true when this station by hash internal server error response has a 2xx status code
func (o *StationByHashInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station by hash internal server error response has a 3xx status code
func (o *StationByHashInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station by hash internal server error response has a 4xx status code
func (o *StationByHashInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station by hash internal server error response has a 5xx status code
func (o *StationByHashInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station by hash internal server error response a status code equal to that given
func (o *StationByHashInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the station by hash internal server error response
func (o *StationByHashInternalServerError) Code() int {
	return 500
}

func (o *StationByHashInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-by-hash][%d] stationByHashInternalServerError ", 500)
}

func (o *StationByHashInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-by-hash][%d] stationByHashInternalServerError ", 500)
}

func (o *StationByHashInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
StationByHashBody ArgStationByHash
swagger:model StationByHashBody
*/
type StationByHashBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationByHashBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this station by hash body
func (o *StationByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationByHashBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this station by hash body based on the context it is used
func (o *StationByHashBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationByHashBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *StationByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationByHashBody) UnmarshalBinary(b []byte) error {
	var res StationByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
