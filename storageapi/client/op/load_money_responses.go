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

// LoadMoneyReader is a Reader for the LoadMoney structure.
type LoadMoneyReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *LoadMoneyReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewLoadMoneyOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewLoadMoneyNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewLoadMoneyInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewLoadMoneyOK creates a LoadMoneyOK with default headers values
func NewLoadMoneyOK() *LoadMoneyOK {
	return &LoadMoneyOK{}
}

/*
LoadMoneyOK describes a response with status code 200, with default header values.

OK
*/
type LoadMoneyOK struct {
	Payload *model.MoneyReport
}

// IsSuccess returns true when this load money o k response has a 2xx status code
func (o *LoadMoneyOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this load money o k response has a 3xx status code
func (o *LoadMoneyOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this load money o k response has a 4xx status code
func (o *LoadMoneyOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this load money o k response has a 5xx status code
func (o *LoadMoneyOK) IsServerError() bool {
	return false
}

// IsCode returns true when this load money o k response a status code equal to that given
func (o *LoadMoneyOK) IsCode(code int) bool {
	return code == 200
}

func (o *LoadMoneyOK) Error() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyOK  %+v", 200, o.Payload)
}

func (o *LoadMoneyOK) String() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyOK  %+v", 200, o.Payload)
}

func (o *LoadMoneyOK) GetPayload() *model.MoneyReport {
	return o.Payload
}

func (o *LoadMoneyOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.MoneyReport)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewLoadMoneyNotFound creates a LoadMoneyNotFound with default headers values
func NewLoadMoneyNotFound() *LoadMoneyNotFound {
	return &LoadMoneyNotFound{}
}

/*
LoadMoneyNotFound describes a response with status code 404, with default header values.

not found
*/
type LoadMoneyNotFound struct {
}

// IsSuccess returns true when this load money not found response has a 2xx status code
func (o *LoadMoneyNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this load money not found response has a 3xx status code
func (o *LoadMoneyNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this load money not found response has a 4xx status code
func (o *LoadMoneyNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this load money not found response has a 5xx status code
func (o *LoadMoneyNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this load money not found response a status code equal to that given
func (o *LoadMoneyNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *LoadMoneyNotFound) Error() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyNotFound ", 404)
}

func (o *LoadMoneyNotFound) String() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyNotFound ", 404)
}

func (o *LoadMoneyNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewLoadMoneyInternalServerError creates a LoadMoneyInternalServerError with default headers values
func NewLoadMoneyInternalServerError() *LoadMoneyInternalServerError {
	return &LoadMoneyInternalServerError{}
}

/*
LoadMoneyInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type LoadMoneyInternalServerError struct {
}

// IsSuccess returns true when this load money internal server error response has a 2xx status code
func (o *LoadMoneyInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this load money internal server error response has a 3xx status code
func (o *LoadMoneyInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this load money internal server error response has a 4xx status code
func (o *LoadMoneyInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this load money internal server error response has a 5xx status code
func (o *LoadMoneyInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this load money internal server error response a status code equal to that given
func (o *LoadMoneyInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *LoadMoneyInternalServerError) Error() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyInternalServerError ", 500)
}

func (o *LoadMoneyInternalServerError) String() string {
	return fmt.Sprintf("[POST /load-money][%d] loadMoneyInternalServerError ", 500)
}

func (o *LoadMoneyInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
LoadMoneyBody ArgLoadMoney
swagger:model LoadMoneyBody
*/
type LoadMoneyBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *LoadMoneyBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this load money body
func (o *LoadMoneyBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadMoneyBody) validateHash(formats strfmt.Registry) error {

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

// ContextValidate validate this load money body based on the context it is used
func (o *LoadMoneyBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadMoneyBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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
func (o *LoadMoneyBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *LoadMoneyBody) UnmarshalBinary(b []byte) error {
	var res LoadMoneyBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
