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

// GetStationWashConfigVarBoolReader is a Reader for the GetStationWashConfigVarBool structure.
type GetStationWashConfigVarBoolReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetStationWashConfigVarBoolReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetStationWashConfigVarBoolOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetStationWashConfigVarBoolNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetStationWashConfigVarBoolInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /get-wash-config-var-bool] getStationWashConfigVarBool", response, response.Code())
	}
}

// NewGetStationWashConfigVarBoolOK creates a GetStationWashConfigVarBoolOK with default headers values
func NewGetStationWashConfigVarBoolOK() *GetStationWashConfigVarBoolOK {
	return &GetStationWashConfigVarBoolOK{}
}

/*
GetStationWashConfigVarBoolOK describes a response with status code 200, with default header values.

OK
*/
type GetStationWashConfigVarBoolOK struct {
	Payload *model.StationConfigVarBool
}

// IsSuccess returns true when this get station wash config var bool o k response has a 2xx status code
func (o *GetStationWashConfigVarBoolOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get station wash config var bool o k response has a 3xx status code
func (o *GetStationWashConfigVarBoolOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var bool o k response has a 4xx status code
func (o *GetStationWashConfigVarBoolOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station wash config var bool o k response has a 5xx status code
func (o *GetStationWashConfigVarBoolOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get station wash config var bool o k response a status code equal to that given
func (o *GetStationWashConfigVarBoolOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get station wash config var bool o k response
func (o *GetStationWashConfigVarBoolOK) Code() int {
	return 200
}

func (o *GetStationWashConfigVarBoolOK) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolOK  %+v", 200, o.Payload)
}

func (o *GetStationWashConfigVarBoolOK) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolOK  %+v", 200, o.Payload)
}

func (o *GetStationWashConfigVarBoolOK) GetPayload() *model.StationConfigVarBool {
	return o.Payload
}

func (o *GetStationWashConfigVarBoolOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationConfigVarBool)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetStationWashConfigVarBoolNotFound creates a GetStationWashConfigVarBoolNotFound with default headers values
func NewGetStationWashConfigVarBoolNotFound() *GetStationWashConfigVarBoolNotFound {
	return &GetStationWashConfigVarBoolNotFound{}
}

/*
GetStationWashConfigVarBoolNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetStationWashConfigVarBoolNotFound struct {
}

// IsSuccess returns true when this get station wash config var bool not found response has a 2xx status code
func (o *GetStationWashConfigVarBoolNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station wash config var bool not found response has a 3xx status code
func (o *GetStationWashConfigVarBoolNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var bool not found response has a 4xx status code
func (o *GetStationWashConfigVarBoolNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get station wash config var bool not found response has a 5xx status code
func (o *GetStationWashConfigVarBoolNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get station wash config var bool not found response a status code equal to that given
func (o *GetStationWashConfigVarBoolNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get station wash config var bool not found response
func (o *GetStationWashConfigVarBoolNotFound) Code() int {
	return 404
}

func (o *GetStationWashConfigVarBoolNotFound) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolNotFound ", 404)
}

func (o *GetStationWashConfigVarBoolNotFound) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolNotFound ", 404)
}

func (o *GetStationWashConfigVarBoolNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetStationWashConfigVarBoolInternalServerError creates a GetStationWashConfigVarBoolInternalServerError with default headers values
func NewGetStationWashConfigVarBoolInternalServerError() *GetStationWashConfigVarBoolInternalServerError {
	return &GetStationWashConfigVarBoolInternalServerError{}
}

/*
GetStationWashConfigVarBoolInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetStationWashConfigVarBoolInternalServerError struct {
}

// IsSuccess returns true when this get station wash config var bool internal server error response has a 2xx status code
func (o *GetStationWashConfigVarBoolInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station wash config var bool internal server error response has a 3xx status code
func (o *GetStationWashConfigVarBoolInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var bool internal server error response has a 4xx status code
func (o *GetStationWashConfigVarBoolInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station wash config var bool internal server error response has a 5xx status code
func (o *GetStationWashConfigVarBoolInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get station wash config var bool internal server error response a status code equal to that given
func (o *GetStationWashConfigVarBoolInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get station wash config var bool internal server error response
func (o *GetStationWashConfigVarBoolInternalServerError) Code() int {
	return 500
}

func (o *GetStationWashConfigVarBoolInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolInternalServerError ", 500)
}

func (o *GetStationWashConfigVarBoolInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-bool][%d] getStationWashConfigVarBoolInternalServerError ", 500)
}

func (o *GetStationWashConfigVarBoolInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetStationWashConfigVarBoolBody ArgGetStationConfigVar
swagger:model GetStationWashConfigVarBoolBody
*/
type GetStationWashConfigVarBoolBody struct {

	// hash
	// Required: true
	Hash *string `json:"hash"`

	// name
	// Required: true
	Name *string `json:"name"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationWashConfigVarBoolBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *string `json:"hash"`

		// name
		// Required: true
		Name *string `json:"name"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Name = props.Name
	return nil
}

// Validate validates this get station wash config var bool body
func (o *GetStationWashConfigVarBoolBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateName(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetStationWashConfigVarBoolBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	return nil
}

func (o *GetStationWashConfigVarBoolBody) validateName(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"name", "body", o.Name); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get station wash config var bool body based on context it is used
func (o *GetStationWashConfigVarBoolBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationWashConfigVarBoolBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationWashConfigVarBoolBody) UnmarshalBinary(b []byte) error {
	var res GetStationWashConfigVarBoolBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
