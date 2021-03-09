// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	strfmt "github.com/go-openapi/strfmt"
)

// SetStationButtonReader is a Reader for the SetStationButton structure.
type SetStationButtonReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SetStationButtonReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 204:
		result := NewSetStationButtonNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 422:
		result := NewSetStationButtonUnprocessableEntity()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	case 500:
		result := NewSetStationButtonInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewSetStationButtonNoContent creates a SetStationButtonNoContent with default headers values
func NewSetStationButtonNoContent() *SetStationButtonNoContent {
	return &SetStationButtonNoContent{}
}

/*SetStationButtonNoContent handles this case with default header values.

OK
*/
type SetStationButtonNoContent struct {
}

func (o *SetStationButtonNoContent) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonNoContent ", 204)
}

func (o *SetStationButtonNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationButtonUnprocessableEntity creates a SetStationButtonUnprocessableEntity with default headers values
func NewSetStationButtonUnprocessableEntity() *SetStationButtonUnprocessableEntity {
	return &SetStationButtonUnprocessableEntity{}
}

/*SetStationButtonUnprocessableEntity handles this case with default header values.

validation error
*/
type SetStationButtonUnprocessableEntity struct {
	Payload string
}

func (o *SetStationButtonUnprocessableEntity) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonUnprocessableEntity  %+v", 422, o.Payload)
}

func (o *SetStationButtonUnprocessableEntity) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewSetStationButtonInternalServerError creates a SetStationButtonInternalServerError with default headers values
func NewSetStationButtonInternalServerError() *SetStationButtonInternalServerError {
	return &SetStationButtonInternalServerError{}
}

/*SetStationButtonInternalServerError handles this case with default header values.

internal error
*/
type SetStationButtonInternalServerError struct {
}

func (o *SetStationButtonInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonInternalServerError ", 500)
}

func (o *SetStationButtonInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*ButtonsItems0 buttons items0
swagger:model ButtonsItems0
*/
type ButtonsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program ID
	ProgramID int64 `json:"programID,omitempty"`
}

// Validate validates this buttons items0
func (o *ButtonsItems0) Validate(formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *ButtonsItems0) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *ButtonsItems0) UnmarshalBinary(b []byte) error {
	var res ButtonsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

/*SetStationButtonBody set station button body
swagger:model SetStationButtonBody
*/
type SetStationButtonBody struct {

	// buttons
	Buttons []*ButtonsItems0 `json:"buttons"`

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// Validate validates this set station button body
func (o *SetStationButtonBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateButtons(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SetStationButtonBody) validateButtons(formats strfmt.Registry) error {

	if swag.IsZero(o.Buttons) { // not required
		return nil
	}

	for i := 0; i < len(o.Buttons); i++ {
		if swag.IsZero(o.Buttons[i]) { // not required
			continue
		}

		if o.Buttons[i] != nil {
			if err := o.Buttons[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (o *SetStationButtonBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", int64(*o.StationID), 1, false); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *SetStationButtonBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SetStationButtonBody) UnmarshalBinary(b []byte) error {
	var res SetStationButtonBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}