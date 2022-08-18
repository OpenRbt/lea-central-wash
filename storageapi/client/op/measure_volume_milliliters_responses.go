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

// MeasureVolumeMillilitersReader is a Reader for the MeasureVolumeMilliliters structure.
type MeasureVolumeMillilitersReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *MeasureVolumeMillilitersReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewMeasureVolumeMillilitersNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewMeasureVolumeMillilitersNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewMeasureVolumeMillilitersInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewMeasureVolumeMillilitersNoContent creates a MeasureVolumeMillilitersNoContent with default headers values
func NewMeasureVolumeMillilitersNoContent() *MeasureVolumeMillilitersNoContent {
	return &MeasureVolumeMillilitersNoContent{}
}

/* MeasureVolumeMillilitersNoContent describes a response with status code 204, with default header values.

OK
*/
type MeasureVolumeMillilitersNoContent struct {
}

// IsSuccess returns true when this measure volume milliliters no content response has a 2xx status code
func (o *MeasureVolumeMillilitersNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this measure volume milliliters no content response has a 3xx status code
func (o *MeasureVolumeMillilitersNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this measure volume milliliters no content response has a 4xx status code
func (o *MeasureVolumeMillilitersNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this measure volume milliliters no content response has a 5xx status code
func (o *MeasureVolumeMillilitersNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this measure volume milliliters no content response a status code equal to that given
func (o *MeasureVolumeMillilitersNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *MeasureVolumeMillilitersNoContent) Error() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersNoContent ", 204)
}

func (o *MeasureVolumeMillilitersNoContent) String() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersNoContent ", 204)
}

func (o *MeasureVolumeMillilitersNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewMeasureVolumeMillilitersNotFound creates a MeasureVolumeMillilitersNotFound with default headers values
func NewMeasureVolumeMillilitersNotFound() *MeasureVolumeMillilitersNotFound {
	return &MeasureVolumeMillilitersNotFound{}
}

/* MeasureVolumeMillilitersNotFound describes a response with status code 404, with default header values.

not found
*/
type MeasureVolumeMillilitersNotFound struct {
	Payload string
}

// IsSuccess returns true when this measure volume milliliters not found response has a 2xx status code
func (o *MeasureVolumeMillilitersNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this measure volume milliliters not found response has a 3xx status code
func (o *MeasureVolumeMillilitersNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this measure volume milliliters not found response has a 4xx status code
func (o *MeasureVolumeMillilitersNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this measure volume milliliters not found response has a 5xx status code
func (o *MeasureVolumeMillilitersNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this measure volume milliliters not found response a status code equal to that given
func (o *MeasureVolumeMillilitersNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *MeasureVolumeMillilitersNotFound) Error() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersNotFound  %+v", 404, o.Payload)
}

func (o *MeasureVolumeMillilitersNotFound) String() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersNotFound  %+v", 404, o.Payload)
}

func (o *MeasureVolumeMillilitersNotFound) GetPayload() string {
	return o.Payload
}

func (o *MeasureVolumeMillilitersNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewMeasureVolumeMillilitersInternalServerError creates a MeasureVolumeMillilitersInternalServerError with default headers values
func NewMeasureVolumeMillilitersInternalServerError() *MeasureVolumeMillilitersInternalServerError {
	return &MeasureVolumeMillilitersInternalServerError{}
}

/* MeasureVolumeMillilitersInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type MeasureVolumeMillilitersInternalServerError struct {
}

// IsSuccess returns true when this measure volume milliliters internal server error response has a 2xx status code
func (o *MeasureVolumeMillilitersInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this measure volume milliliters internal server error response has a 3xx status code
func (o *MeasureVolumeMillilitersInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this measure volume milliliters internal server error response has a 4xx status code
func (o *MeasureVolumeMillilitersInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this measure volume milliliters internal server error response has a 5xx status code
func (o *MeasureVolumeMillilitersInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this measure volume milliliters internal server error response a status code equal to that given
func (o *MeasureVolumeMillilitersInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *MeasureVolumeMillilitersInternalServerError) Error() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersInternalServerError ", 500)
}

func (o *MeasureVolumeMillilitersInternalServerError) String() string {
	return fmt.Sprintf("[POST /run-dispenser][%d] measureVolumeMillilitersInternalServerError ", 500)
}

func (o *MeasureVolumeMillilitersInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*MeasureVolumeMillilitersBody ArgMeasureVolumeMilliliters
swagger:model MeasureVolumeMillilitersBody
*/
type MeasureVolumeMillilitersBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// volume
	// Required: true
	Volume *int64 `json:"volume"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *MeasureVolumeMillilitersBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// volume
		// Required: true
		Volume *int64 `json:"volume"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Volume = props.Volume
	return nil
}

// Validate validates this measure volume milliliters body
func (o *MeasureVolumeMillilitersBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateVolume(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *MeasureVolumeMillilitersBody) validateHash(formats strfmt.Registry) error {

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

func (o *MeasureVolumeMillilitersBody) validateVolume(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"volume", "body", o.Volume); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this measure volume milliliters body based on the context it is used
func (o *MeasureVolumeMillilitersBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *MeasureVolumeMillilitersBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *MeasureVolumeMillilitersBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *MeasureVolumeMillilitersBody) UnmarshalBinary(b []byte) error {
	var res MeasureVolumeMillilitersBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
