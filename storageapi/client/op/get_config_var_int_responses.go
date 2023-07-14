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

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// GetConfigVarIntReader is a Reader for the GetConfigVarInt structure.
type GetConfigVarIntReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetConfigVarIntReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetConfigVarIntOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetConfigVarIntNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetConfigVarIntInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /get-config-var-int] getConfigVarInt", response, response.Code())
	}
}

// NewGetConfigVarIntOK creates a GetConfigVarIntOK with default headers values
func NewGetConfigVarIntOK() *GetConfigVarIntOK {
	return &GetConfigVarIntOK{}
}

/*
GetConfigVarIntOK describes a response with status code 200, with default header values.

OK
*/
type GetConfigVarIntOK struct {
	Payload *model.ConfigVarInt
}

// IsSuccess returns true when this get config var int o k response has a 2xx status code
func (o *GetConfigVarIntOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get config var int o k response has a 3xx status code
func (o *GetConfigVarIntOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var int o k response has a 4xx status code
func (o *GetConfigVarIntOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var int o k response has a 5xx status code
func (o *GetConfigVarIntOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var int o k response a status code equal to that given
func (o *GetConfigVarIntOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get config var int o k response
func (o *GetConfigVarIntOK) Code() int {
	return 200
}

func (o *GetConfigVarIntOK) Error() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarIntOK) String() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarIntOK) GetPayload() *model.ConfigVarInt {
	return o.Payload
}

func (o *GetConfigVarIntOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.ConfigVarInt)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetConfigVarIntNotFound creates a GetConfigVarIntNotFound with default headers values
func NewGetConfigVarIntNotFound() *GetConfigVarIntNotFound {
	return &GetConfigVarIntNotFound{}
}

/*
GetConfigVarIntNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetConfigVarIntNotFound struct {
}

// IsSuccess returns true when this get config var int not found response has a 2xx status code
func (o *GetConfigVarIntNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var int not found response has a 3xx status code
func (o *GetConfigVarIntNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var int not found response has a 4xx status code
func (o *GetConfigVarIntNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get config var int not found response has a 5xx status code
func (o *GetConfigVarIntNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var int not found response a status code equal to that given
func (o *GetConfigVarIntNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get config var int not found response
func (o *GetConfigVarIntNotFound) Code() int {
	return 404
}

func (o *GetConfigVarIntNotFound) Error() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntNotFound ", 404)
}

func (o *GetConfigVarIntNotFound) String() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntNotFound ", 404)
}

func (o *GetConfigVarIntNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetConfigVarIntInternalServerError creates a GetConfigVarIntInternalServerError with default headers values
func NewGetConfigVarIntInternalServerError() *GetConfigVarIntInternalServerError {
	return &GetConfigVarIntInternalServerError{}
}

/*
GetConfigVarIntInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetConfigVarIntInternalServerError struct {
}

// IsSuccess returns true when this get config var int internal server error response has a 2xx status code
func (o *GetConfigVarIntInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var int internal server error response has a 3xx status code
func (o *GetConfigVarIntInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var int internal server error response has a 4xx status code
func (o *GetConfigVarIntInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var int internal server error response has a 5xx status code
func (o *GetConfigVarIntInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get config var int internal server error response a status code equal to that given
func (o *GetConfigVarIntInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get config var int internal server error response
func (o *GetConfigVarIntInternalServerError) Code() int {
	return 500
}

func (o *GetConfigVarIntInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntInternalServerError ", 500)
}

func (o *GetConfigVarIntInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-config-var-int][%d] getConfigVarIntInternalServerError ", 500)
}

func (o *GetConfigVarIntInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetConfigVarIntBody ArgGetConfigVar
swagger:model GetConfigVarIntBody
*/
type GetConfigVarIntBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetConfigVarIntBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// name
		Name string `json:"name,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Name = props.Name
	return nil
}

// Validate validates this get config var int body
func (o *GetConfigVarIntBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var int body based on context it is used
func (o *GetConfigVarIntBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarIntBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarIntBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarIntBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
