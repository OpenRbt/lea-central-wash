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

// GetStationDiscountsReader is a Reader for the GetStationDiscounts structure.
type GetStationDiscountsReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *GetStationDiscountsReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewGetStationDiscountsOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewGetStationDiscountsNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewGetStationDiscountsInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewGetStationDiscountsOK creates a GetStationDiscountsOK with default headers values
func NewGetStationDiscountsOK() *GetStationDiscountsOK {
	return &GetStationDiscountsOK{}
}

/*
GetStationDiscountsOK describes a response with status code 200, with default header values.

OK
*/
type GetStationDiscountsOK struct {
	Payload model.StationDiscounts
}

// IsSuccess returns true when this get station discounts o k response has a 2xx status code
func (o *GetStationDiscountsOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this get station discounts o k response has a 3xx status code
func (o *GetStationDiscountsOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station discounts o k response has a 4xx status code
func (o *GetStationDiscountsOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station discounts o k response has a 5xx status code
func (o *GetStationDiscountsOK) IsServerError() bool {
	return false
}

// IsCode returns true when this get station discounts o k response a status code equal to that given
func (o *GetStationDiscountsOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the get station discounts o k response
func (o *GetStationDiscountsOK) Code() int {
	return 200
}

func (o *GetStationDiscountsOK) Error() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsOK  %+v", 200, o.Payload)
}

func (o *GetStationDiscountsOK) String() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsOK  %+v", 200, o.Payload)
}

func (o *GetStationDiscountsOK) GetPayload() model.StationDiscounts {
	return o.Payload
}

func (o *GetStationDiscountsOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewGetStationDiscountsNotFound creates a GetStationDiscountsNotFound with default headers values
func NewGetStationDiscountsNotFound() *GetStationDiscountsNotFound {
	return &GetStationDiscountsNotFound{}
}

/*
GetStationDiscountsNotFound describes a response with status code 404, with default header values.

Not found
*/
type GetStationDiscountsNotFound struct {
}

// IsSuccess returns true when this get station discounts not found response has a 2xx status code
func (o *GetStationDiscountsNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station discounts not found response has a 3xx status code
func (o *GetStationDiscountsNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station discounts not found response has a 4xx status code
func (o *GetStationDiscountsNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this get station discounts not found response has a 5xx status code
func (o *GetStationDiscountsNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this get station discounts not found response a status code equal to that given
func (o *GetStationDiscountsNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the get station discounts not found response
func (o *GetStationDiscountsNotFound) Code() int {
	return 404
}

func (o *GetStationDiscountsNotFound) Error() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsNotFound ", 404)
}

func (o *GetStationDiscountsNotFound) String() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsNotFound ", 404)
}

func (o *GetStationDiscountsNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewGetStationDiscountsInternalServerError creates a GetStationDiscountsInternalServerError with default headers values
func NewGetStationDiscountsInternalServerError() *GetStationDiscountsInternalServerError {
	return &GetStationDiscountsInternalServerError{}
}

/*
GetStationDiscountsInternalServerError describes a response with status code 500, with default header values.

Internal error
*/
type GetStationDiscountsInternalServerError struct {
}

// IsSuccess returns true when this get station discounts internal server error response has a 2xx status code
func (o *GetStationDiscountsInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this get station discounts internal server error response has a 3xx status code
func (o *GetStationDiscountsInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this get station discounts internal server error response has a 4xx status code
func (o *GetStationDiscountsInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this get station discounts internal server error response has a 5xx status code
func (o *GetStationDiscountsInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this get station discounts internal server error response a status code equal to that given
func (o *GetStationDiscountsInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the get station discounts internal server error response
func (o *GetStationDiscountsInternalServerError) Code() int {
	return 500
}

func (o *GetStationDiscountsInternalServerError) Error() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsInternalServerError ", 500)
}

func (o *GetStationDiscountsInternalServerError) String() string {
	return fmt.Sprintf("[POST /get-station-discounts][%d] getStationDiscountsInternalServerError ", 500)
}

func (o *GetStationDiscountsInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
GetStationDiscountsBody ArgGetStationDiscounts
swagger:model GetStationDiscountsBody
*/
type GetStationDiscountsBody struct {

	// hash
	Hash string `json:"hash,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *GetStationDiscountsBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		Hash string `json:"hash,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	return nil
}

// Validate validates this get station discounts body
func (o *GetStationDiscountsBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this get station discounts body based on context it is used
func (o *GetStationDiscountsBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *GetStationDiscountsBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *GetStationDiscountsBody) UnmarshalBinary(b []byte) error {
	var res GetStationDiscountsBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
