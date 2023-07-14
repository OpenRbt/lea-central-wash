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
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
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
		return nil, runtime.NewAPIError("[POST /set-station-button] setStationButton", response, response.Code())
	}
}

// NewSetStationButtonNoContent creates a SetStationButtonNoContent with default headers values
func NewSetStationButtonNoContent() *SetStationButtonNoContent {
	return &SetStationButtonNoContent{}
}

/*
SetStationButtonNoContent describes a response with status code 204, with default header values.

OK
*/
type SetStationButtonNoContent struct {
}

// IsSuccess returns true when this set station button no content response has a 2xx status code
func (o *SetStationButtonNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this set station button no content response has a 3xx status code
func (o *SetStationButtonNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station button no content response has a 4xx status code
func (o *SetStationButtonNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station button no content response has a 5xx status code
func (o *SetStationButtonNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this set station button no content response a status code equal to that given
func (o *SetStationButtonNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the set station button no content response
func (o *SetStationButtonNoContent) Code() int {
	return 204
}

func (o *SetStationButtonNoContent) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonNoContent ", 204)
}

func (o *SetStationButtonNoContent) String() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonNoContent ", 204)
}

func (o *SetStationButtonNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSetStationButtonUnprocessableEntity creates a SetStationButtonUnprocessableEntity with default headers values
func NewSetStationButtonUnprocessableEntity() *SetStationButtonUnprocessableEntity {
	return &SetStationButtonUnprocessableEntity{}
}

/*
SetStationButtonUnprocessableEntity describes a response with status code 422, with default header values.

validation error
*/
type SetStationButtonUnprocessableEntity struct {
	Payload string
}

// IsSuccess returns true when this set station button unprocessable entity response has a 2xx status code
func (o *SetStationButtonUnprocessableEntity) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station button unprocessable entity response has a 3xx status code
func (o *SetStationButtonUnprocessableEntity) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station button unprocessable entity response has a 4xx status code
func (o *SetStationButtonUnprocessableEntity) IsClientError() bool {
	return true
}

// IsServerError returns true when this set station button unprocessable entity response has a 5xx status code
func (o *SetStationButtonUnprocessableEntity) IsServerError() bool {
	return false
}

// IsCode returns true when this set station button unprocessable entity response a status code equal to that given
func (o *SetStationButtonUnprocessableEntity) IsCode(code int) bool {
	return code == 422
}

// Code gets the status code for the set station button unprocessable entity response
func (o *SetStationButtonUnprocessableEntity) Code() int {
	return 422
}

func (o *SetStationButtonUnprocessableEntity) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonUnprocessableEntity  %+v", 422, o.Payload)
}

func (o *SetStationButtonUnprocessableEntity) String() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonUnprocessableEntity  %+v", 422, o.Payload)
}

func (o *SetStationButtonUnprocessableEntity) GetPayload() string {
	return o.Payload
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

/*
SetStationButtonInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SetStationButtonInternalServerError struct {
}

// IsSuccess returns true when this set station button internal server error response has a 2xx status code
func (o *SetStationButtonInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this set station button internal server error response has a 3xx status code
func (o *SetStationButtonInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this set station button internal server error response has a 4xx status code
func (o *SetStationButtonInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this set station button internal server error response has a 5xx status code
func (o *SetStationButtonInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this set station button internal server error response a status code equal to that given
func (o *SetStationButtonInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the set station button internal server error response
func (o *SetStationButtonInternalServerError) Code() int {
	return 500
}

func (o *SetStationButtonInternalServerError) Error() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonInternalServerError ", 500)
}

func (o *SetStationButtonInternalServerError) String() string {
	return fmt.Sprintf("[POST /set-station-button][%d] setStationButtonInternalServerError ", 500)
}

func (o *SetStationButtonInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
SetStationButtonBody ArgSetStationButton
swagger:model SetStationButtonBody
*/
type SetStationButtonBody struct {

	// buttons
	Buttons []*SetStationButtonParamsBodyButtonsItems0 `json:"buttons"`

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SetStationButtonBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// buttons
		Buttons []*SetStationButtonParamsBodyButtonsItems0 `json:"buttons"`

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

	o.Buttons = props.Buttons
	o.StationID = props.StationID
	return nil
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
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
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

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this set station button body based on the context it is used
func (o *SetStationButtonBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateButtons(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SetStationButtonBody) contextValidateButtons(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(o.Buttons); i++ {

		if o.Buttons[i] != nil {

			if swag.IsZero(o.Buttons[i]) { // not required
				return nil
			}

			if err := o.Buttons[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("args" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

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

/*
SetStationButtonParamsBodyButtonsItems0 set station button params body buttons items0
swagger:model SetStationButtonParamsBodyButtonsItems0
*/
type SetStationButtonParamsBodyButtonsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program ID
	ProgramID int64 `json:"programID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SetStationButtonParamsBodyButtonsItems0) UnmarshalJSON(data []byte) error {
	var props struct {

		// button ID
		ButtonID int64 `json:"buttonID,omitempty"`

		// program ID
		ProgramID int64 `json:"programID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ButtonID = props.ButtonID
	o.ProgramID = props.ProgramID
	return nil
}

// Validate validates this set station button params body buttons items0
func (o *SetStationButtonParamsBodyButtonsItems0) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this set station button params body buttons items0 based on context it is used
func (o *SetStationButtonParamsBodyButtonsItems0) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *SetStationButtonParamsBodyButtonsItems0) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SetStationButtonParamsBodyButtonsItems0) UnmarshalBinary(b []byte) error {
	var res SetStationButtonParamsBodyButtonsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
