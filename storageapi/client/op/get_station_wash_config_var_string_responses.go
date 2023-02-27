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

// GetStationWashConfigVarStringReader is a Reader for the GetStationWashConfigVarString structure.
type GetStationWashConfigVarStringReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetStationWashConfigVarStringReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetStationWashConfigVarStringOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetStationWashConfigVarStringNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetStationWashConfigVarStringInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetStationWashConfigVarStringOK creates a GetStationWashConfigVarStringOK with default headers values
func NewGetStationWashConfigVarStringOK() *GetStationWashConfigVarStringOK {
	return &GetStationWashConfigVarStringOK{}
}

/*
GetStationWashConfigVarStringOK describes a response with status code 200, with default header values.

OK
*/
type GetStationWashConfigVarStringOK struct {
	Payload *model.StationConfigVarString
}

// IsSuccess returns true when this get station wash config var string o k response has a 2xx status code
func (o *GetStationWashConfigVarStringOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get station wash config var string o k response has a 3xx status code
func (o *GetStationWashConfigVarStringOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var string o k response has a 4xx status code
func (o *GetStationWashConfigVarStringOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station wash config var string o k response has a 5xx status code
func (o *GetStationWashConfigVarStringOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get station wash config var string o k response a status code equal to that given
func (o *GetStationWashConfigVarStringOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get station wash config var string o k response
func (o *GetStationWashConfigVarStringOK) Code() int {
	return 200
}

func (o *GetStationWashConfigVarStringOK) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringOK  %+v", 200, o.Payload)
}

func (o *GetStationWashConfigVarStringOK) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringOK  %+v", 200, o.Payload)
}

func (o *GetStationWashConfigVarStringOK) GetPayload() *model.StationConfigVarString {
	return o.Payload
}

func (o *GetStationWashConfigVarStringOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationConfigVarString)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetStationWashConfigVarStringNotFound creates a GetStationWashConfigVarStringNotFound with default headers values
func NewGetStationWashConfigVarStringNotFound() *GetStationWashConfigVarStringNotFound {
	return &GetStationWashConfigVarStringNotFound{}
}

/*
GetStationWashConfigVarStringNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetStationWashConfigVarStringNotFound struct {
}

// IsSuccess returns true when this get station wash config var string not found response has a 2xx status code
func (o *GetStationWashConfigVarStringNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station wash config var string not found response has a 3xx status code
func (o *GetStationWashConfigVarStringNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var string not found response has a 4xx status code
func (o *GetStationWashConfigVarStringNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get station wash config var string not found response has a 5xx status code
func (o *GetStationWashConfigVarStringNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get station wash config var string not found response a status code equal to that given
func (o *GetStationWashConfigVarStringNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get station wash config var string not found response
func (o *GetStationWashConfigVarStringNotFound) Code() int {
	return 404
}

func (o *GetStationWashConfigVarStringNotFound) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringNotFound ", 404)
}

func (o *GetStationWashConfigVarStringNotFound) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringNotFound ", 404)
}

func (o *GetStationWashConfigVarStringNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetStationWashConfigVarStringInternalServerError creates a GetStationWashConfigVarStringInternalServerError with default headers values
func NewGetStationWashConfigVarStringInternalServerError() *GetStationWashConfigVarStringInternalServerError {
	return &GetStationWashConfigVarStringInternalServerError{}
}

/*
GetStationWashConfigVarStringInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetStationWashConfigVarStringInternalServerError struct {
}

// IsSuccess returns true when this get station wash config var string internal server error response has a 2xx status code
func (o *GetStationWashConfigVarStringInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station wash config var string internal server error response has a 3xx status code
func (o *GetStationWashConfigVarStringInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station wash config var string internal server error response has a 4xx status code
func (o *GetStationWashConfigVarStringInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station wash config var string internal server error response has a 5xx status code
func (o *GetStationWashConfigVarStringInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get station wash config var string internal server error response a status code equal to that given
func (o *GetStationWashConfigVarStringInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get station wash config var string internal server error response
func (o *GetStationWashConfigVarStringInternalServerError) Code() int {
	return 500
}

func (o *GetStationWashConfigVarStringInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringInternalServerError ", 500)
}

func (o *GetStationWashConfigVarStringInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-wash-config-var-string][%d] getStationWashConfigVarStringInternalServerError ", 500)
}

func (o *GetStationWashConfigVarStringInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetStationWashConfigVarStringBody ArgGetStationConfigVar
swagger:model GetStationWashConfigVarStringBody
*/
type GetStationWashConfigVarStringBody struct {

	// hash
	Hash string `json:"hash,omitempty"`

	// name
	// Required: true
	Name *string `json:"name"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationWashConfigVarStringBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		Hash string `json:"hash,omitempty"`

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

// Validate validates this get station wash config var string body
func (o *GetStationWashConfigVarStringBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateName(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *GetStationWashConfigVarStringBody) validateName(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"name", "body", o.Name); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this get station wash config var string body based on context it is used
func (o *GetStationWashConfigVarStringBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationWashConfigVarStringBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationWashConfigVarStringBody) UnmarshalBinary(b []byte) error {
	var res GetStationWashConfigVarStringBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
