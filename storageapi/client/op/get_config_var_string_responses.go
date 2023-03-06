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

// GetConfigVarStringReader is a Reader for the GetConfigVarString structure.
type GetConfigVarStringReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetConfigVarStringReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetConfigVarStringOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetConfigVarStringNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetConfigVarStringInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetConfigVarStringOK creates a GetConfigVarStringOK with default headers values
func NewGetConfigVarStringOK() *GetConfigVarStringOK {
	return &GetConfigVarStringOK{}
}

/*
GetConfigVarStringOK describes a response with status code 200, with default header values.

OK
*/
type GetConfigVarStringOK struct {
	Payload *model.ConfigVarString
}

// IsSuccess returns true when this get config var string o k response has a 2xx status code
func (o *GetConfigVarStringOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get config var string o k response has a 3xx status code
func (o *GetConfigVarStringOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var string o k response has a 4xx status code
func (o *GetConfigVarStringOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var string o k response has a 5xx status code
func (o *GetConfigVarStringOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var string o k response a status code equal to that given
func (o *GetConfigVarStringOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get config var string o k response
func (o *GetConfigVarStringOK) Code() int {
	return 200
}

func (o *GetConfigVarStringOK) Error() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarStringOK) String() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarStringOK) GetPayload() *model.ConfigVarString {
	return o.Payload
}

func (o *GetConfigVarStringOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.ConfigVarString)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetConfigVarStringNotFound creates a GetConfigVarStringNotFound with default headers values
func NewGetConfigVarStringNotFound() *GetConfigVarStringNotFound {
	return &GetConfigVarStringNotFound{}
}

/*
GetConfigVarStringNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetConfigVarStringNotFound struct {
}

// IsSuccess returns true when this get config var string not found response has a 2xx status code
func (o *GetConfigVarStringNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var string not found response has a 3xx status code
func (o *GetConfigVarStringNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var string not found response has a 4xx status code
func (o *GetConfigVarStringNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get config var string not found response has a 5xx status code
func (o *GetConfigVarStringNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var string not found response a status code equal to that given
func (o *GetConfigVarStringNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get config var string not found response
func (o *GetConfigVarStringNotFound) Code() int {
	return 404
}

func (o *GetConfigVarStringNotFound) Error() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringNotFound ", 404)
}

func (o *GetConfigVarStringNotFound) String() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringNotFound ", 404)
}

func (o *GetConfigVarStringNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetConfigVarStringInternalServerError creates a GetConfigVarStringInternalServerError with default headers values
func NewGetConfigVarStringInternalServerError() *GetConfigVarStringInternalServerError {
	return &GetConfigVarStringInternalServerError{}
}

/*
GetConfigVarStringInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetConfigVarStringInternalServerError struct {
}

// IsSuccess returns true when this get config var string internal server error response has a 2xx status code
func (o *GetConfigVarStringInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var string internal server error response has a 3xx status code
func (o *GetConfigVarStringInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var string internal server error response has a 4xx status code
func (o *GetConfigVarStringInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var string internal server error response has a 5xx status code
func (o *GetConfigVarStringInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get config var string internal server error response a status code equal to that given
func (o *GetConfigVarStringInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get config var string internal server error response
func (o *GetConfigVarStringInternalServerError) Code() int {
	return 500
}

func (o *GetConfigVarStringInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringInternalServerError ", 500)
}

func (o *GetConfigVarStringInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-config-var-string][%d] getConfigVarStringInternalServerError ", 500)
}

func (o *GetConfigVarStringInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetConfigVarStringBody ArgGetConfigVar
swagger:model GetConfigVarStringBody
*/
type GetConfigVarStringBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetConfigVarStringBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this get config var string body
func (o *GetConfigVarStringBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var string body based on context it is used
func (o *GetConfigVarStringBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarStringBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarStringBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarStringBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
