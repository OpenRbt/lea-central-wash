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

// CardReaderConfigReader is a Reader for the CardReaderConfig structure.
type CardReaderConfigReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *CardReaderConfigReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewCardReaderConfigOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewCardReaderConfigNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewCardReaderConfigInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewCardReaderConfigOK creates a CardReaderConfigOK with default headers values
func NewCardReaderConfigOK() *CardReaderConfigOK {
	return &CardReaderConfigOK{}
}

/* CardReaderConfigOK describes a response with status code 200, with default header values.

OK
*/
type CardReaderConfigOK struct {
	Payload *model.CardReaderConfig
}

func (o *CardReaderConfigOK) Error() string {
	return fmt.Sprintf("[POST /card-reader-config][%d] cardReaderConfigOK  %+v", 200, o.Payload)
}
func (o *CardReaderConfigOK) GetPayload() *model.CardReaderConfig {
	return o.Payload
}

func (o *CardReaderConfigOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.CardReaderConfig)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewCardReaderConfigNotFound creates a CardReaderConfigNotFound with default headers values
func NewCardReaderConfigNotFound() *CardReaderConfigNotFound {
	return &CardReaderConfigNotFound{}
}

/* CardReaderConfigNotFound describes a response with status code 404, with default header values.

not found
*/
type CardReaderConfigNotFound struct {
}

func (o *CardReaderConfigNotFound) Error() string {
	return fmt.Sprintf("[POST /card-reader-config][%d] cardReaderConfigNotFound ", 404)
}

func (o *CardReaderConfigNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewCardReaderConfigInternalServerError creates a CardReaderConfigInternalServerError with default headers values
func NewCardReaderConfigInternalServerError() *CardReaderConfigInternalServerError {
	return &CardReaderConfigInternalServerError{}
}

/* CardReaderConfigInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type CardReaderConfigInternalServerError struct {
}

func (o *CardReaderConfigInternalServerError) Error() string {
	return fmt.Sprintf("[POST /card-reader-config][%d] cardReaderConfigInternalServerError ", 500)
}

func (o *CardReaderConfigInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*CardReaderConfigBody ArgCardReaderConfig
swagger:model CardReaderConfigBody
*/
type CardReaderConfigBody struct {

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CardReaderConfigBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// station ID
		// Required: true
		// Minimum: 1
		StationID *int64 `json:"stationID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.StationID = props.StationID
	return nil
}

// Validate validates this card reader config body
func (o *CardReaderConfigBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CardReaderConfigBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this card reader config body based on context it is used
func (o *CardReaderConfigBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *CardReaderConfigBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CardReaderConfigBody) UnmarshalBinary(b []byte) error {
	var res CardReaderConfigBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
