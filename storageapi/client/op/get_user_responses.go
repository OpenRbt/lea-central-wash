// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// GetUserReader is a Reader for the GetUser structure.
type GetUserReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetUserReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetUserOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewGetUserUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewGetUserForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetUserOK creates a GetUserOK with default headers values
func NewGetUserOK() *GetUserOK {
	return &GetUserOK{}
}

/*
GetUserOK describes a response with status code 200, with default header values.

OK
*/
type GetUserOK struct {
	Payload *model.UserConfig
}

// IsSuccess returns true when this get user o k response has a 2xx status code
func (o *GetUserOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get user o k response has a 3xx status code
func (o *GetUserOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get user o k response has a 4xx status code
func (o *GetUserOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get user o k response has a 5xx status code
func (o *GetUserOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get user o k response a status code equal to that given
func (o *GetUserOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get user o k response
func (o *GetUserOK) Code() int {
	return 200
}

func (o *GetUserOK) Error() string {
	return fmt.Sprintf("[GET /user][%d] getUserOK  %+v", 200, o.Payload)
}

func (o *GetUserOK) String() string {
	return fmt.Sprintf("[GET /user][%d] getUserOK  %+v", 200, o.Payload)
}

func (o *GetUserOK) GetPayload() *model.UserConfig {
	return o.Payload
}

func (o *GetUserOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.UserConfig)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetUserUnauthorized creates a GetUserUnauthorized with default headers values
func NewGetUserUnauthorized() *GetUserUnauthorized {
	return &GetUserUnauthorized{}
}

/*
GetUserUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type GetUserUnauthorized struct {
}

// IsSuccess returns true when this get user unauthorized response has a 2xx status code
func (o *GetUserUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get user unauthorized response has a 3xx status code
func (o *GetUserUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get user unauthorized response has a 4xx status code
func (o *GetUserUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this get user unauthorized response has a 5xx status code
func (o *GetUserUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this get user unauthorized response a status code equal to that given
func (o *GetUserUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the get user unauthorized response
func (o *GetUserUnauthorized) Code() int {
	return 401
}

func (o *GetUserUnauthorized) Error() string {
	return fmt.Sprintf("[GET /user][%d] getUserUnauthorized ", 401)
}

func (o *GetUserUnauthorized) String() string {
	return fmt.Sprintf("[GET /user][%d] getUserUnauthorized ", 401)
}

func (o *GetUserUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetUserForbidden creates a GetUserForbidden with default headers values
func NewGetUserForbidden() *GetUserForbidden {
	return &GetUserForbidden{}
}

/*
GetUserForbidden describes a response with status code 403, with default header values.

Access forbidden
*/
type GetUserForbidden struct {
}

// IsSuccess returns true when this get user forbidden response has a 2xx status code
func (o *GetUserForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get user forbidden response has a 3xx status code
func (o *GetUserForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get user forbidden response has a 4xx status code
func (o *GetUserForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this get user forbidden response has a 5xx status code
func (o *GetUserForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this get user forbidden response a status code equal to that given
func (o *GetUserForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the get user forbidden response
func (o *GetUserForbidden) Code() int {
	return 403
}

func (o *GetUserForbidden) Error() string {
	return fmt.Sprintf("[GET /user][%d] getUserForbidden ", 403)
}

func (o *GetUserForbidden) String() string {
	return fmt.Sprintf("[GET /user][%d] getUserForbidden ", 403)
}

func (o *GetUserForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
