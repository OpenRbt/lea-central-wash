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

// CardReaderConfigByHashReader is a Reader for the CardReaderConfigByHash structure.
type CardReaderConfigByHashReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *CardReaderConfigByHashReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewCardReaderConfigByHashOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewCardReaderConfigByHashNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewCardReaderConfigByHashInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewCardReaderConfigByHashOK creates a CardReaderConfigByHashOK with default headers values
func NewCardReaderConfigByHashOK() *CardReaderConfigByHashOK {
	return &CardReaderConfigByHashOK{}
}

/*
CardReaderConfigByHashOK describes a response with status code 200, with default header values.

OK
*/
type CardReaderConfigByHashOK struct {
	Payload *model.CardReaderConfig
}

// IsSuccess returns true when this card reader config by hash o k response has a 2xx status code
func (o *CardReaderConfigByHashOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this card reader config by hash o k response has a 3xx status code
func (o *CardReaderConfigByHashOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this card reader config by hash o k response has a 4xx status code
func (o *CardReaderConfigByHashOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this card reader config by hash o k response has a 5xx status code
func (o *CardReaderConfigByHashOK) IsServerError() bool {
	return false
}

// IsCode returns true when this card reader config by hash o k response a status code equal to that given
func (o *CardReaderConfigByHashOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the card reader config by hash o k response
func (o *CardReaderConfigByHashOK) Code() int {
	return 200
}

func (o *CardReaderConfigByHashOK) Error() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashOK  %+v", 200, o.Payload)
}

func (o *CardReaderConfigByHashOK) String() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashOK  %+v", 200, o.Payload)
}

func (o *CardReaderConfigByHashOK) GetPayload() *model.CardReaderConfig {
	return o.Payload
}

func (o *CardReaderConfigByHashOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.CardReaderConfig)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewCardReaderConfigByHashNotFound creates a CardReaderConfigByHashNotFound with default headers values
func NewCardReaderConfigByHashNotFound() *CardReaderConfigByHashNotFound {
	return &CardReaderConfigByHashNotFound{}
}

/*
CardReaderConfigByHashNotFound describes a response with status code 404, with default header values.

not found
*/
type CardReaderConfigByHashNotFound struct {
}

// IsSuccess returns true when this card reader config by hash not found response has a 2xx status code
func (o *CardReaderConfigByHashNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this card reader config by hash not found response has a 3xx status code
func (o *CardReaderConfigByHashNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this card reader config by hash not found response has a 4xx status code
func (o *CardReaderConfigByHashNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this card reader config by hash not found response has a 5xx status code
func (o *CardReaderConfigByHashNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this card reader config by hash not found response a status code equal to that given
func (o *CardReaderConfigByHashNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the card reader config by hash not found response
func (o *CardReaderConfigByHashNotFound) Code() int {
	return 404
}

func (o *CardReaderConfigByHashNotFound) Error() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashNotFound ", 404)
}

func (o *CardReaderConfigByHashNotFound) String() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashNotFound ", 404)
}

func (o *CardReaderConfigByHashNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewCardReaderConfigByHashInternalServerError creates a CardReaderConfigByHashInternalServerError with default headers values
func NewCardReaderConfigByHashInternalServerError() *CardReaderConfigByHashInternalServerError {
	return &CardReaderConfigByHashInternalServerError{}
}

/*
CardReaderConfigByHashInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type CardReaderConfigByHashInternalServerError struct {
}

// IsSuccess returns true when this card reader config by hash internal server error response has a 2xx status code
func (o *CardReaderConfigByHashInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this card reader config by hash internal server error response has a 3xx status code
func (o *CardReaderConfigByHashInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this card reader config by hash internal server error response has a 4xx status code
func (o *CardReaderConfigByHashInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this card reader config by hash internal server error response has a 5xx status code
func (o *CardReaderConfigByHashInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this card reader config by hash internal server error response a status code equal to that given
func (o *CardReaderConfigByHashInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the card reader config by hash internal server error response
func (o *CardReaderConfigByHashInternalServerError) Code() int {
	return 500
}

func (o *CardReaderConfigByHashInternalServerError) Error() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashInternalServerError ", 500)
}

func (o *CardReaderConfigByHashInternalServerError) String() string {
	return fmt.Sprintf("[POST /card-reader-config-by-hash][%d] cardReaderConfigByHashInternalServerError ", 500)
}

func (o *CardReaderConfigByHashInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
CardReaderConfigByHashBody ArgCardReaderConfigByCash
swagger:model CardReaderConfigByHashBody
*/
type CardReaderConfigByHashBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CardReaderConfigByHashBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this card reader config by hash body
func (o *CardReaderConfigByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CardReaderConfigByHashBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this card reader config by hash body based on the context it is used
func (o *CardReaderConfigByHashBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CardReaderConfigByHashBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *CardReaderConfigByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CardReaderConfigByHashBody) UnmarshalBinary(b []byte) error {
	var res CardReaderConfigByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
