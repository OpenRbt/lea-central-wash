// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// AddServiceAmountReader is a Reader for the AddServiceAmount structure.
type AddServiceAmountReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *AddServiceAmountReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewAddServiceAmountNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewAddServiceAmountNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewAddServiceAmountInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewAddServiceAmountNoContent creates a AddServiceAmountNoContent with default headers values
func NewAddServiceAmountNoContent() *AddServiceAmountNoContent {
	return &AddServiceAmountNoContent{}
}

/* AddServiceAmountNoContent describes a response with status code 204, with default header values.

OK
*/
type AddServiceAmountNoContent struct {
}

func (o *AddServiceAmountNoContent) Error() string {
	return fmt.Sprintf("[POST /add-service-amount][%d] addServiceAmountNoContent ", 204)
}

func (o *AddServiceAmountNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAddServiceAmountNotFound creates a AddServiceAmountNotFound with default headers values
func NewAddServiceAmountNotFound() *AddServiceAmountNotFound {
	return &AddServiceAmountNotFound{}
}

/* AddServiceAmountNotFound describes a response with status code 404, with default header values.

not found
*/
type AddServiceAmountNotFound struct {
}

func (o *AddServiceAmountNotFound) Error() string {
	return fmt.Sprintf("[POST /add-service-amount][%d] addServiceAmountNotFound ", 404)
}

func (o *AddServiceAmountNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAddServiceAmountInternalServerError creates a AddServiceAmountInternalServerError with default headers values
func NewAddServiceAmountInternalServerError() *AddServiceAmountInternalServerError {
	return &AddServiceAmountInternalServerError{}
}

/* AddServiceAmountInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type AddServiceAmountInternalServerError struct {
}

func (o *AddServiceAmountInternalServerError) Error() string {
	return fmt.Sprintf("[POST /add-service-amount][%d] addServiceAmountInternalServerError ", 500)
}

func (o *AddServiceAmountInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*AddServiceAmountBody ArgAddServiceAmount
swagger:model AddServiceAmountBody
*/
type AddServiceAmountBody struct {

	// amount
	Amount int64 `json:"amount,omitempty"`

	// hash
	Hash string `json:"hash,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AddServiceAmountBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// amount
		Amount int64 `json:"amount,omitempty"`

		// hash
		Hash string `json:"hash,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Amount = props.Amount
	o.Hash = props.Hash
	return nil
}

// Validate validates this add service amount body
func (o *AddServiceAmountBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this add service amount body based on context it is used
func (o *AddServiceAmountBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AddServiceAmountBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AddServiceAmountBody) UnmarshalBinary(b []byte) error {
	var res AddServiceAmountBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
