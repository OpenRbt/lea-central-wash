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

// GetLevelReader is a Reader for the GetLevel structure.
type GetLevelReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetLevelReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetLevelOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 204:
		result := NewGetLevelNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetLevelNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetLevelInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /getLevel] GetLevel", response, response.Code())
	}
}

// NewGetLevelOK creates a GetLevelOK with default headers values
func NewGetLevelOK() *GetLevelOK {
	return &GetLevelOK{}
}

/*
GetLevelOK describes a response with status code 200, with default header values.

OK
*/
type GetLevelOK struct {
	Payload *GetLevelOKBody
}

// IsSuccess returns true when this get level o k response has a 2xx status code
func (o *GetLevelOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get level o k response has a 3xx status code
func (o *GetLevelOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get level o k response has a 4xx status code
func (o *GetLevelOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get level o k response has a 5xx status code
func (o *GetLevelOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get level o k response a status code equal to that given
func (o *GetLevelOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get level o k response
func (o *GetLevelOK) Code() int {
	return 200
}

func (o *GetLevelOK) Error() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelOK  %+v", 200, o.Payload)
}

func (o *GetLevelOK) String() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelOK  %+v", 200, o.Payload)
}

func (o *GetLevelOK) GetPayload() *GetLevelOKBody {
	return o.Payload
}

func (o *GetLevelOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(GetLevelOKBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetLevelNoContent creates a GetLevelNoContent with default headers values
func NewGetLevelNoContent() *GetLevelNoContent {
	return &GetLevelNoContent{}
}

/*
GetLevelNoContent describes a response with status code 204, with default header values.

OK
*/
type GetLevelNoContent struct {
}

// IsSuccess returns true when this get level no content response has a 2xx status code
func (o *GetLevelNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get level no content response has a 3xx status code
func (o *GetLevelNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get level no content response has a 4xx status code
func (o *GetLevelNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this get level no content response has a 5xx status code
func (o *GetLevelNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this get level no content response a status code equal to that given
func (o *GetLevelNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the get level no content response
func (o *GetLevelNoContent) Code() int {
	return 204
}

func (o *GetLevelNoContent) Error() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelNoContent ", 204)
}

func (o *GetLevelNoContent) String() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelNoContent ", 204)
}

func (o *GetLevelNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetLevelNotFound creates a GetLevelNotFound with default headers values
func NewGetLevelNotFound() *GetLevelNotFound {
	return &GetLevelNotFound{}
}

/*
GetLevelNotFound describes a response with status code 404, with default header values.

not found
*/
type GetLevelNotFound struct {
	Payload string
}

// IsSuccess returns true when this get level not found response has a 2xx status code
func (o *GetLevelNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get level not found response has a 3xx status code
func (o *GetLevelNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get level not found response has a 4xx status code
func (o *GetLevelNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get level not found response has a 5xx status code
func (o *GetLevelNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get level not found response a status code equal to that given
func (o *GetLevelNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get level not found response
func (o *GetLevelNotFound) Code() int {
	return 404
}

func (o *GetLevelNotFound) Error() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelNotFound  %+v", 404, o.Payload)
}

func (o *GetLevelNotFound) String() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelNotFound  %+v", 404, o.Payload)
}

func (o *GetLevelNotFound) GetPayload() string {
	return o.Payload
}

func (o *GetLevelNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetLevelInternalServerError creates a GetLevelInternalServerError with default headers values
func NewGetLevelInternalServerError() *GetLevelInternalServerError {
	return &GetLevelInternalServerError{}
}

/*
GetLevelInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type GetLevelInternalServerError struct {
}

// IsSuccess returns true when this get level internal server error response has a 2xx status code
func (o *GetLevelInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get level internal server error response has a 3xx status code
func (o *GetLevelInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get level internal server error response has a 4xx status code
func (o *GetLevelInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get level internal server error response has a 5xx status code
func (o *GetLevelInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get level internal server error response a status code equal to that given
func (o *GetLevelInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get level internal server error response
func (o *GetLevelInternalServerError) Code() int {
	return 500
}

func (o *GetLevelInternalServerError) Error() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelInternalServerError ", 500)
}

func (o *GetLevelInternalServerError) String() string {
	return fmt.Sprintf("[POST /getLevel][%d] getLevelInternalServerError ", 500)
}

func (o *GetLevelInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetLevelBody ArgGetLevel
swagger:model GetLevelBody
*/
type GetLevelBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetLevelBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this get level body
func (o *GetLevelBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this get level body based on the context it is used
func (o *GetLevelBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *GetLevelBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetLevelBody) UnmarshalBinary(b []byte) error {
	var res GetLevelBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

/*
GetLevelOKBody ResponseGetLevel
swagger:model GetLevelOKBody
*/
type GetLevelOKBody struct {

	// level
	// Required: true
	Level *int64 `json:"level"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetLevelOKBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// level
		// Required: true
		Level *int64 `json:"level"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Level = props.Level
	return nil
}

// Validate validates this get level o k body
func (o *GetLevelOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateLevel(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetLevelOKBody) validateLevel(formats strfmt.Registry) error {

	if err := validate.Required("getLevelOK"+"."+"level", "body", o.Level); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get level o k body based on context it is used
func (o *GetLevelOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetLevelOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetLevelOKBody) UnmarshalBinary(b []byte) error {
	var res GetLevelOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
