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

// StationReportDatesReader is a Reader for the StationReportDates structure.
type StationReportDatesReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationReportDatesReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStationReportDatesOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewStationReportDatesNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewStationReportDatesInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewStationReportDatesOK creates a StationReportDatesOK with default headers values
func NewStationReportDatesOK() *StationReportDatesOK {
	return &StationReportDatesOK{}
}

/*
StationReportDatesOK describes a response with status code 200, with default header values.

OK
*/
type StationReportDatesOK struct {
	Payload *model.StationReport
}

// IsSuccess returns true when this station report dates o k response has a 2xx status code
func (o *StationReportDatesOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station report dates o k response has a 3xx status code
func (o *StationReportDatesOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station report dates o k response has a 4xx status code
func (o *StationReportDatesOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station report dates o k response has a 5xx status code
func (o *StationReportDatesOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station report dates o k response a status code equal to that given
func (o *StationReportDatesOK) IsCode(code int) bool {
	return code == 200
}

func (o *StationReportDatesOK) Error() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesOK  %+v", 200, o.Payload)
}

func (o *StationReportDatesOK) String() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesOK  %+v", 200, o.Payload)
}

func (o *StationReportDatesOK) GetPayload() *model.StationReport {
	return o.Payload
}

func (o *StationReportDatesOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationReport)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationReportDatesNotFound creates a StationReportDatesNotFound with default headers values
func NewStationReportDatesNotFound() *StationReportDatesNotFound {
	return &StationReportDatesNotFound{}
}

/*
StationReportDatesNotFound describes a response with status code 404, with default header values.

not found
*/
type StationReportDatesNotFound struct {
}

// IsSuccess returns true when this station report dates not found response has a 2xx status code
func (o *StationReportDatesNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station report dates not found response has a 3xx status code
func (o *StationReportDatesNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station report dates not found response has a 4xx status code
func (o *StationReportDatesNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this station report dates not found response has a 5xx status code
func (o *StationReportDatesNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this station report dates not found response a status code equal to that given
func (o *StationReportDatesNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *StationReportDatesNotFound) Error() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesNotFound ", 404)
}

func (o *StationReportDatesNotFound) String() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesNotFound ", 404)
}

func (o *StationReportDatesNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationReportDatesInternalServerError creates a StationReportDatesInternalServerError with default headers values
func NewStationReportDatesInternalServerError() *StationReportDatesInternalServerError {
	return &StationReportDatesInternalServerError{}
}

/*
StationReportDatesInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationReportDatesInternalServerError struct {
}

// IsSuccess returns true when this station report dates internal server error response has a 2xx status code
func (o *StationReportDatesInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station report dates internal server error response has a 3xx status code
func (o *StationReportDatesInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station report dates internal server error response has a 4xx status code
func (o *StationReportDatesInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station report dates internal server error response has a 5xx status code
func (o *StationReportDatesInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station report dates internal server error response a status code equal to that given
func (o *StationReportDatesInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *StationReportDatesInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesInternalServerError ", 500)
}

func (o *StationReportDatesInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-report-dates][%d] stationReportDatesInternalServerError ", 500)
}

func (o *StationReportDatesInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
StationReportDatesBody ArgStationReportDates
swagger:model StationReportDatesBody
*/
type StationReportDatesBody struct {

	// Unix time
	// Required: true
	EndDate *int64 `json:"endDate"`

	// id
	// Required: true
	ID *int64 `json:"id"`

	// Unix time
	// Required: true
	StartDate *int64 `json:"startDate"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationReportDatesBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// Unix time
		// Required: true
		EndDate *int64 `json:"endDate"`

		// id
		// Required: true
		ID *int64 `json:"id"`

		// Unix time
		// Required: true
		StartDate *int64 `json:"startDate"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.EndDate = props.EndDate
	o.ID = props.ID
	o.StartDate = props.StartDate
	return nil
}

// Validate validates this station report dates body
func (o *StationReportDatesBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateEndDate(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStartDate(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationReportDatesBody) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"endDate", "body", o.EndDate); err != nil {
		return err
	}

	return nil
}

func (o *StationReportDatesBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

func (o *StationReportDatesBody) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startDate", "body", o.StartDate); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this station report dates body based on context it is used
func (o *StationReportDatesBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationReportDatesBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationReportDatesBody) UnmarshalBinary(b []byte) error {
	var res StationReportDatesBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
