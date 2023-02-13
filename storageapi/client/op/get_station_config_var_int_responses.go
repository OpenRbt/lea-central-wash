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

// GetStationConfigVarIntReader is a Reader for the GetStationConfigVarInt structure.
type GetStationConfigVarIntReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetStationConfigVarIntReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetStationConfigVarIntOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetStationConfigVarIntNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetStationConfigVarIntInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetStationConfigVarIntOK creates a GetStationConfigVarIntOK with default headers values
func NewGetStationConfigVarIntOK() *GetStationConfigVarIntOK {
	return &GetStationConfigVarIntOK{}
}

/*
GetStationConfigVarIntOK describes a response with status code 200, with default header values.

OK
*/
type GetStationConfigVarIntOK struct {
	Payload *model.StationConfigVarInt
}

// IsSuccess returns true when this get station config var int o k response has a 2xx status code
func (o *GetStationConfigVarIntOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get station config var int o k response has a 3xx status code
func (o *GetStationConfigVarIntOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station config var int o k response has a 4xx status code
func (o *GetStationConfigVarIntOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station config var int o k response has a 5xx status code
func (o *GetStationConfigVarIntOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get station config var int o k response a status code equal to that given
func (o *GetStationConfigVarIntOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get station config var int o k response
func (o *GetStationConfigVarIntOK) Code() int {
	return 200
}

func (o *GetStationConfigVarIntOK) Error() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntOK  %+v", 200, o.Payload)
}

func (o *GetStationConfigVarIntOK) String() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntOK  %+v", 200, o.Payload)
}

func (o *GetStationConfigVarIntOK) GetPayload() *model.StationConfigVarInt {
	return o.Payload
}

func (o *GetStationConfigVarIntOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationConfigVarInt)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetStationConfigVarIntNotFound creates a GetStationConfigVarIntNotFound with default headers values
func NewGetStationConfigVarIntNotFound() *GetStationConfigVarIntNotFound {
	return &GetStationConfigVarIntNotFound{}
}

/*
GetStationConfigVarIntNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetStationConfigVarIntNotFound struct {
}

// IsSuccess returns true when this get station config var int not found response has a 2xx status code
func (o *GetStationConfigVarIntNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station config var int not found response has a 3xx status code
func (o *GetStationConfigVarIntNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station config var int not found response has a 4xx status code
func (o *GetStationConfigVarIntNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get station config var int not found response has a 5xx status code
func (o *GetStationConfigVarIntNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get station config var int not found response a status code equal to that given
func (o *GetStationConfigVarIntNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get station config var int not found response
func (o *GetStationConfigVarIntNotFound) Code() int {
	return 404
}

func (o *GetStationConfigVarIntNotFound) Error() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntNotFound ", 404)
}

func (o *GetStationConfigVarIntNotFound) String() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntNotFound ", 404)
}

func (o *GetStationConfigVarIntNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetStationConfigVarIntInternalServerError creates a GetStationConfigVarIntInternalServerError with default headers values
func NewGetStationConfigVarIntInternalServerError() *GetStationConfigVarIntInternalServerError {
	return &GetStationConfigVarIntInternalServerError{}
}

/*
GetStationConfigVarIntInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetStationConfigVarIntInternalServerError struct {
}

// IsSuccess returns true when this get station config var int internal server error response has a 2xx status code
func (o *GetStationConfigVarIntInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station config var int internal server error response has a 3xx status code
func (o *GetStationConfigVarIntInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station config var int internal server error response has a 4xx status code
func (o *GetStationConfigVarIntInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station config var int internal server error response has a 5xx status code
func (o *GetStationConfigVarIntInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get station config var int internal server error response a status code equal to that given
func (o *GetStationConfigVarIntInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get station config var int internal server error response
func (o *GetStationConfigVarIntInternalServerError) Code() int {
	return 500
}

func (o *GetStationConfigVarIntInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntInternalServerError ", 500)
}

func (o *GetStationConfigVarIntInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-station-config-var-int][%d] getStationConfigVarIntInternalServerError ", 500)
}

func (o *GetStationConfigVarIntInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetStationConfigVarIntBody ArgGetStationConfigVar
swagger:model GetStationConfigVarIntBody
*/
type GetStationConfigVarIntBody struct {

	// name
	Name string `json:"name,omitempty"`

	// station ID
	StationID int64 `json:"stationID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationConfigVarIntBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// name
		Name string `json:"name,omitempty"`

		// station ID
		StationID int64 `json:"stationID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Name = props.Name
	o.StationID = props.StationID
	return nil
}

// Validate validates this get station config var int body
func (o *GetStationConfigVarIntBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get station config var int body based on context it is used
func (o *GetStationConfigVarIntBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationConfigVarIntBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationConfigVarIntBody) UnmarshalBinary(b []byte) error {
	var res GetStationConfigVarIntBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
