// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// ResetStationStatReader is a Reader for the ResetStationStat structure.
type ResetStationStatReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *ResetStationStatReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewResetStationStatNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewResetStationStatUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewResetStationStatForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewResetStationStatInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewResetStationStatNoContent creates a ResetStationStatNoContent with default headers values
func NewResetStationStatNoContent() *ResetStationStatNoContent {
	return &ResetStationStatNoContent{}
}

/*
ResetStationStatNoContent describes a response with status code 204, with default header values.

OK
*/
type ResetStationStatNoContent struct {
}

// IsSuccess returns true when this reset station stat no content response has a 2xx status code
func (o *ResetStationStatNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this reset station stat no content response has a 3xx status code
func (o *ResetStationStatNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this reset station stat no content response has a 4xx status code
func (o *ResetStationStatNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this reset station stat no content response has a 5xx status code
func (o *ResetStationStatNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this reset station stat no content response a status code equal to that given
func (o *ResetStationStatNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the reset station stat no content response
func (o *ResetStationStatNoContent) Code() int {
	return 204
}

func (o *ResetStationStatNoContent) Error() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatNoContent ", 204)
}

func (o *ResetStationStatNoContent) String() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatNoContent ", 204)
}

func (o *ResetStationStatNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewResetStationStatUnauthorized creates a ResetStationStatUnauthorized with default headers values
func NewResetStationStatUnauthorized() *ResetStationStatUnauthorized {
	return &ResetStationStatUnauthorized{}
}

/*
ResetStationStatUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type ResetStationStatUnauthorized struct {
}

// IsSuccess returns true when this reset station stat unauthorized response has a 2xx status code
func (o *ResetStationStatUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this reset station stat unauthorized response has a 3xx status code
func (o *ResetStationStatUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this reset station stat unauthorized response has a 4xx status code
func (o *ResetStationStatUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this reset station stat unauthorized response has a 5xx status code
func (o *ResetStationStatUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this reset station stat unauthorized response a status code equal to that given
func (o *ResetStationStatUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the reset station stat unauthorized response
func (o *ResetStationStatUnauthorized) Code() int {
	return 401
}

func (o *ResetStationStatUnauthorized) Error() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatUnauthorized ", 401)
}

func (o *ResetStationStatUnauthorized) String() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatUnauthorized ", 401)
}

func (o *ResetStationStatUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewResetStationStatForbidden creates a ResetStationStatForbidden with default headers values
func NewResetStationStatForbidden() *ResetStationStatForbidden {
	return &ResetStationStatForbidden{}
}

/*
ResetStationStatForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type ResetStationStatForbidden struct {
}

// IsSuccess returns true when this reset station stat forbidden response has a 2xx status code
func (o *ResetStationStatForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this reset station stat forbidden response has a 3xx status code
func (o *ResetStationStatForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this reset station stat forbidden response has a 4xx status code
func (o *ResetStationStatForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this reset station stat forbidden response has a 5xx status code
func (o *ResetStationStatForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this reset station stat forbidden response a status code equal to that given
func (o *ResetStationStatForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the reset station stat forbidden response
func (o *ResetStationStatForbidden) Code() int {
	return 403
}

func (o *ResetStationStatForbidden) Error() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatForbidden ", 403)
}

func (o *ResetStationStatForbidden) String() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatForbidden ", 403)
}

func (o *ResetStationStatForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewResetStationStatInternalServerError creates a ResetStationStatInternalServerError with default headers values
func NewResetStationStatInternalServerError() *ResetStationStatInternalServerError {
	return &ResetStationStatInternalServerError{}
}

/*
ResetStationStatInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type ResetStationStatInternalServerError struct {
}

// IsSuccess returns true when this reset station stat internal server error response has a 2xx status code
func (o *ResetStationStatInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this reset station stat internal server error response has a 3xx status code
func (o *ResetStationStatInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this reset station stat internal server error response has a 4xx status code
func (o *ResetStationStatInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this reset station stat internal server error response has a 5xx status code
func (o *ResetStationStatInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this reset station stat internal server error response a status code equal to that given
func (o *ResetStationStatInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the reset station stat internal server error response
func (o *ResetStationStatInternalServerError) Code() int {
	return 500
}

func (o *ResetStationStatInternalServerError) Error() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatInternalServerError ", 500)
}

func (o *ResetStationStatInternalServerError) String() string {
	return fmt.Sprintf("[POST /reset-station-stat][%d] resetStationStatInternalServerError ", 500)
}

func (o *ResetStationStatInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
ResetStationStatBody ArgResetStationStat
swagger:model ResetStationStatBody
*/
type ResetStationStatBody struct {

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *ResetStationStatBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// station ID
		// Required: true
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

// Validate validates this reset station stat body
func (o *ResetStationStatBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *ResetStationStatBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this reset station stat body based on context it is used
func (o *ResetStationStatBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *ResetStationStatBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *ResetStationStatBody) UnmarshalBinary(b []byte) error {
	var res ResetStationStatBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
