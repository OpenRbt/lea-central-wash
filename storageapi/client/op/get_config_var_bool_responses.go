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

// GetConfigVarBoolReader is a Reader for the GetConfigVarBool structure.
type GetConfigVarBoolReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetConfigVarBoolReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetConfigVarBoolOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetConfigVarBoolNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetConfigVarBoolInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetConfigVarBoolOK creates a GetConfigVarBoolOK with default headers values
func NewGetConfigVarBoolOK() *GetConfigVarBoolOK {
	return &GetConfigVarBoolOK{}
}

/* GetConfigVarBoolOK describes a response with status code 200, with default header values.

OK
*/
type GetConfigVarBoolOK struct {
	Payload *model.ConfigVarBool
}

// IsSuccess returns true when this get config var bool o k response has a 2xx status code
func (o *GetConfigVarBoolOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get config var bool o k response has a 3xx status code
func (o *GetConfigVarBoolOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var bool o k response has a 4xx status code
func (o *GetConfigVarBoolOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var bool o k response has a 5xx status code
func (o *GetConfigVarBoolOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var bool o k response a status code equal to that given
func (o *GetConfigVarBoolOK) IsCode(code int) bool {
	return code == 200
}

func (o *GetConfigVarBoolOK) Error() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarBoolOK) String() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolOK  %+v", 200, o.Payload)
}

func (o *GetConfigVarBoolOK) GetPayload() *model.ConfigVarBool {
	return o.Payload
}

func (o *GetConfigVarBoolOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.ConfigVarBool)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetConfigVarBoolNotFound creates a GetConfigVarBoolNotFound with default headers values
func NewGetConfigVarBoolNotFound() *GetConfigVarBoolNotFound {
	return &GetConfigVarBoolNotFound{}
}

/* GetConfigVarBoolNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetConfigVarBoolNotFound struct {
}

// IsSuccess returns true when this get config var bool not found response has a 2xx status code
func (o *GetConfigVarBoolNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var bool not found response has a 3xx status code
func (o *GetConfigVarBoolNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var bool not found response has a 4xx status code
func (o *GetConfigVarBoolNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get config var bool not found response has a 5xx status code
func (o *GetConfigVarBoolNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get config var bool not found response a status code equal to that given
func (o *GetConfigVarBoolNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *GetConfigVarBoolNotFound) Error() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolNotFound ", 404)
}

func (o *GetConfigVarBoolNotFound) String() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolNotFound ", 404)
}

func (o *GetConfigVarBoolNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetConfigVarBoolInternalServerError creates a GetConfigVarBoolInternalServerError with default headers values
func NewGetConfigVarBoolInternalServerError() *GetConfigVarBoolInternalServerError {
	return &GetConfigVarBoolInternalServerError{}
}

/* GetConfigVarBoolInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetConfigVarBoolInternalServerError struct {
}

// IsSuccess returns true when this get config var bool internal server error response has a 2xx status code
func (o *GetConfigVarBoolInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get config var bool internal server error response has a 3xx status code
func (o *GetConfigVarBoolInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get config var bool internal server error response has a 4xx status code
func (o *GetConfigVarBoolInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get config var bool internal server error response has a 5xx status code
func (o *GetConfigVarBoolInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get config var bool internal server error response a status code equal to that given
func (o *GetConfigVarBoolInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *GetConfigVarBoolInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolInternalServerError ", 500)
}

func (o *GetConfigVarBoolInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-config-var-bool][%d] getConfigVarBoolInternalServerError ", 500)
}

func (o *GetConfigVarBoolInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*GetConfigVarBoolBody ArgGetConfigVar
swagger:model GetConfigVarBoolBody
*/
type GetConfigVarBoolBody struct {

	// name
	Name string `json:"name,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetConfigVarBoolBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this get config var bool body
func (o *GetConfigVarBoolBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get config var bool body based on context it is used
func (o *GetConfigVarBoolBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetConfigVarBoolBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetConfigVarBoolBody) UnmarshalBinary(b []byte) error {
	var res GetConfigVarBoolBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
