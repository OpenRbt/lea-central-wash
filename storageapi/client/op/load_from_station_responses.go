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

// LoadFromStationReader is a Reader for the LoadFromStation structure.
type LoadFromStationReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *LoadFromStationReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewLoadFromStationOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewLoadFromStationNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewLoadFromStationInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewLoadFromStationOK creates a LoadFromStationOK with default headers values
func NewLoadFromStationOK() *LoadFromStationOK {
	return &LoadFromStationOK{}
}

/* LoadFromStationOK describes a response with status code 200, with default header values.

OK
*/
type LoadFromStationOK struct {
	Payload string
}

func (o *LoadFromStationOK) Error() string {
	return fmt.Sprintf("[POST /load-from-station][%d] loadFromStationOK  %+v", 200, o.Payload)
}
func (o *LoadFromStationOK) GetPayload() string {
	return o.Payload
}

func (o *LoadFromStationOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewLoadFromStationNotFound creates a LoadFromStationNotFound with default headers values
func NewLoadFromStationNotFound() *LoadFromStationNotFound {
	return &LoadFromStationNotFound{}
}

/* LoadFromStationNotFound describes a response with status code 404, with default header values.

not found
*/
type LoadFromStationNotFound struct {
}

func (o *LoadFromStationNotFound) Error() string {
	return fmt.Sprintf("[POST /load-from-station][%d] loadFromStationNotFound ", 404)
}

func (o *LoadFromStationNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewLoadFromStationInternalServerError creates a LoadFromStationInternalServerError with default headers values
func NewLoadFromStationInternalServerError() *LoadFromStationInternalServerError {
	return &LoadFromStationInternalServerError{}
}

/* LoadFromStationInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type LoadFromStationInternalServerError struct {
}

func (o *LoadFromStationInternalServerError) Error() string {
	return fmt.Sprintf("[POST /load-from-station][%d] loadFromStationInternalServerError ", 500)
}

func (o *LoadFromStationInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*LoadFromStationBody load from station body
swagger:model LoadFromStationBody
*/
type LoadFromStationBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// key
	// Required: true
	// Min Length: 1
	Key *string `json:"key"`

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *LoadFromStationBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// key
		// Required: true
		// Min Length: 1
		Key *string `json:"key"`

		// station ID
		// Required: true
		StationID *int64 `json:"stationID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Key = props.Key
	o.StationID = props.StationID
	return nil
}

// Validate validates this load from station body
func (o *LoadFromStationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateKey(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadFromStationBody) validateHash(formats strfmt.Registry) error {

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
			}
			return err
		}
	}

	return nil
}

func (o *LoadFromStationBody) validateKey(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"key", "body", o.Key); err != nil {
		return err
	}

	if err := validate.MinLength("args"+"."+"key", "body", *o.Key, 1); err != nil {
		return err
	}

	return nil
}

func (o *LoadFromStationBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this load from station body based on the context it is used
func (o *LoadFromStationBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadFromStationBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {
		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *LoadFromStationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *LoadFromStationBody) UnmarshalBinary(b []byte) error {
	var res LoadFromStationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
