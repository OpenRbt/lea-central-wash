// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	strfmt "github.com/go-openapi/strfmt"

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationReportReader is a Reader for the StationReport structure.
type StationReportReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationReportReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 200:
		result := NewStationReportOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 404:
		result := NewStationReportNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	case 500:
		result := NewStationReportInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewStationReportOK creates a StationReportOK with default headers values
func NewStationReportOK() *StationReportOK {
	return &StationReportOK{}
}

/*StationReportOK handles this case with default header values.

OK
*/
type StationReportOK struct {
	Payload *model.StationReport
}

func (o *StationReportOK) Error() string {
	return fmt.Sprintf("[POST /station-report][%d] stationReportOK  %+v", 200, o.Payload)
}

func (o *StationReportOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationReport)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationReportNotFound creates a StationReportNotFound with default headers values
func NewStationReportNotFound() *StationReportNotFound {
	return &StationReportNotFound{}
}

/*StationReportNotFound handles this case with default header values.

not found
*/
type StationReportNotFound struct {
}

func (o *StationReportNotFound) Error() string {
	return fmt.Sprintf("[POST /station-report][%d] stationReportNotFound ", 404)
}

func (o *StationReportNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationReportInternalServerError creates a StationReportInternalServerError with default headers values
func NewStationReportInternalServerError() *StationReportInternalServerError {
	return &StationReportInternalServerError{}
}

/*StationReportInternalServerError handles this case with default header values.

internal error
*/
type StationReportInternalServerError struct {
}

func (o *StationReportInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-report][%d] stationReportInternalServerError ", 500)
}

func (o *StationReportInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*StationReportBody station report body
swagger:model StationReportBody
*/
type StationReportBody struct {

	// end date
	// Required: true
	// Format: date
	EndDate *strfmt.Date `json:"endDate"`

	// id
	// Required: true
	ID *int64 `json:"id"`

	// start date
	// Required: true
	// Format: date
	StartDate *strfmt.Date `json:"startDate"`
}

// Validate validates this station report body
func (o *StationReportBody) Validate(formats strfmt.Registry) error {
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

func (o *StationReportBody) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"endDate", "body", o.EndDate); err != nil {
		return err
	}

	if err := validate.FormatOf("args"+"."+"endDate", "body", "date", o.EndDate.String(), formats); err != nil {
		return err
	}

	return nil
}

func (o *StationReportBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

func (o *StationReportBody) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startDate", "body", o.StartDate); err != nil {
		return err
	}

	if err := validate.FormatOf("args"+"."+"startDate", "body", "date", o.StartDate.String(), formats); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationReportBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationReportBody) UnmarshalBinary(b []byte) error {
	var res StationReportBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
