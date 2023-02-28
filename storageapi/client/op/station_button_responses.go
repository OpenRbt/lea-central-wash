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

// StationButtonReader is a Reader for the StationButton structure.
type StationButtonReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationButtonReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStationButtonOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 500:
		result := NewStationButtonInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewStationButtonOK creates a StationButtonOK with default headers values
func NewStationButtonOK() *StationButtonOK {
	return &StationButtonOK{}
}

/*
StationButtonOK describes a response with status code 200, with default header values.

OK
*/
type StationButtonOK struct {
	Payload *StationButtonOKBody
}

// IsSuccess returns true when this station button o k response has a 2xx status code
func (o *StationButtonOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station button o k response has a 3xx status code
func (o *StationButtonOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station button o k response has a 4xx status code
func (o *StationButtonOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station button o k response has a 5xx status code
func (o *StationButtonOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station button o k response a status code equal to that given
func (o *StationButtonOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the station button o k response
func (o *StationButtonOK) Code() int {
	return 200
}

func (o *StationButtonOK) Error() string {
	return fmt.Sprintf("[POST /station-button][%d] stationButtonOK  %+v", 200, o.Payload)
}

func (o *StationButtonOK) String() string {
	return fmt.Sprintf("[POST /station-button][%d] stationButtonOK  %+v", 200, o.Payload)
}

func (o *StationButtonOK) GetPayload() *StationButtonOKBody {
	return o.Payload
}

func (o *StationButtonOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(StationButtonOKBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationButtonInternalServerError creates a StationButtonInternalServerError with default headers values
func NewStationButtonInternalServerError() *StationButtonInternalServerError {
	return &StationButtonInternalServerError{}
}

/*
StationButtonInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationButtonInternalServerError struct {
}

// IsSuccess returns true when this station button internal server error response has a 2xx status code
func (o *StationButtonInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station button internal server error response has a 3xx status code
func (o *StationButtonInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station button internal server error response has a 4xx status code
func (o *StationButtonInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station button internal server error response has a 5xx status code
func (o *StationButtonInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station button internal server error response a status code equal to that given
func (o *StationButtonInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the station button internal server error response
func (o *StationButtonInternalServerError) Code() int {
	return 500
}

func (o *StationButtonInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-button][%d] stationButtonInternalServerError ", 500)
}

func (o *StationButtonInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-button][%d] stationButtonInternalServerError ", 500)
}

func (o *StationButtonInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
StationButtonBody ArgStationButton
swagger:model StationButtonBody
*/
type StationButtonBody struct {

	// station ID
	// Required: true
	// Minimum: 1
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationButtonBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this station button body
func (o *StationButtonBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	if err := validate.MinimumInt("args"+"."+"stationID", "body", *o.StationID, 1, false); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station button body based on context it is used
func (o *StationButtonBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonBody) UnmarshalBinary(b []byte) error {
	var res StationButtonBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

/*
StationButtonOKBody ResponseStationButton
swagger:model StationButtonOKBody
*/
type StationButtonOKBody struct {

	// buttons
	Buttons []*StationButtonOKBodyButtonsItems0 `json:"buttons"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationButtonOKBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// buttons
		Buttons []*StationButtonOKBodyButtonsItems0 `json:"buttons"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Buttons = props.Buttons
	return nil
}

// Validate validates this station button o k body
func (o *StationButtonOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateButtons(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonOKBody) validateButtons(formats strfmt.Registry) error {
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
					return ve.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// ContextValidate validate this station button o k body based on the context it is used
func (o *StationButtonOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateButtons(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationButtonOKBody) contextValidateButtons(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(o.Buttons); i++ {

		if o.Buttons[i] != nil {
			if err := o.Buttons[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationButtonOK" + "." + "buttons" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonOKBody) UnmarshalBinary(b []byte) error {
	var res StationButtonOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

/*
StationButtonOKBodyButtonsItems0 station button o k body buttons items0
swagger:model StationButtonOKBodyButtonsItems0
*/
type StationButtonOKBodyButtonsItems0 struct {

	// button ID
	ButtonID int64 `json:"buttonID,omitempty"`

	// program ID
	ProgramID int64 `json:"programID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationButtonOKBodyButtonsItems0) UnmarshalJSON(data []byte) error {
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

// Validate validates this station button o k body buttons items0
func (o *StationButtonOKBodyButtonsItems0) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this station button o k body buttons items0 based on context it is used
func (o *StationButtonOKBodyButtonsItems0) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationButtonOKBodyButtonsItems0) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationButtonOKBodyButtonsItems0) UnmarshalBinary(b []byte) error {
	var res StationButtonOKBodyButtonsItems0
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
