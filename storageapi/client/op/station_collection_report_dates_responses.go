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

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationCollectionReportDatesReader is a Reader for the StationCollectionReportDates structure.
type StationCollectionReportDatesReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationCollectionReportDatesReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 200:
		result := NewStationCollectionReportDatesOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 404:
		result := NewStationCollectionReportDatesNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	case 500:
		result := NewStationCollectionReportDatesInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewStationCollectionReportDatesOK creates a StationCollectionReportDatesOK with default headers values
func NewStationCollectionReportDatesOK() *StationCollectionReportDatesOK {
	return &StationCollectionReportDatesOK{}
}

/*StationCollectionReportDatesOK handles this case with default header values.

OK
*/
type StationCollectionReportDatesOK struct {
	Payload *StationCollectionReportDatesOKBody
}

func (o *StationCollectionReportDatesOK) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesOK  %+v", 200, o.Payload)
}

func (o *StationCollectionReportDatesOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(StationCollectionReportDatesOKBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationCollectionReportDatesNotFound creates a StationCollectionReportDatesNotFound with default headers values
func NewStationCollectionReportDatesNotFound() *StationCollectionReportDatesNotFound {
	return &StationCollectionReportDatesNotFound{}
}

/*StationCollectionReportDatesNotFound handles this case with default header values.

not found
*/
type StationCollectionReportDatesNotFound struct {
}

func (o *StationCollectionReportDatesNotFound) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesNotFound ", 404)
}

func (o *StationCollectionReportDatesNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationCollectionReportDatesInternalServerError creates a StationCollectionReportDatesInternalServerError with default headers values
func NewStationCollectionReportDatesInternalServerError() *StationCollectionReportDatesInternalServerError {
	return &StationCollectionReportDatesInternalServerError{}
}

/*StationCollectionReportDatesInternalServerError handles this case with default header values.

internal error
*/
type StationCollectionReportDatesInternalServerError struct {
}

func (o *StationCollectionReportDatesInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesInternalServerError ", 500)
}

func (o *StationCollectionReportDatesInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*StationCollectionReportDatesBody station collection report dates body
swagger:model StationCollectionReportDatesBody
*/
type StationCollectionReportDatesBody struct {

	// Unix time
	// Required: true
	EndDate *int64 `json:"endDate"`

	// Unix time
	// Required: true
	StartDate *int64 `json:"startDate"`

	// station ID
	StationID int64 `json:"stationID,omitempty"`
}

// Validate validates this station collection report dates body
func (o *StationCollectionReportDatesBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateEndDate(formats); err != nil {
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

func (o *StationCollectionReportDatesBody) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"endDate", "body", o.EndDate); err != nil {
		return err
	}

	return nil
}

func (o *StationCollectionReportDatesBody) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"startDate", "body", o.StartDate); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationCollectionReportDatesBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationCollectionReportDatesBody) UnmarshalBinary(b []byte) error {
	var res StationCollectionReportDatesBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

/*StationCollectionReportDatesOKBody station collection report dates o k body
swagger:model StationCollectionReportDatesOKBody
*/
type StationCollectionReportDatesOKBody struct {

	// collection reports
	// Required: true
	CollectionReports []*model.CollectionReportWithUser `json:"collectionReports"`
}

// Validate validates this station collection report dates o k body
func (o *StationCollectionReportDatesOKBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateCollectionReports(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationCollectionReportDatesOKBody) validateCollectionReports(formats strfmt.Registry) error {

	if err := validate.Required("stationCollectionReportDatesOK"+"."+"collectionReports", "body", o.CollectionReports); err != nil {
		return err
	}

	for i := 0; i < len(o.CollectionReports); i++ {
		if swag.IsZero(o.CollectionReports[i]) { // not required
			continue
		}

		if o.CollectionReports[i] != nil {
			if err := o.CollectionReports[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("stationCollectionReportDatesOK" + "." + "collectionReports" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationCollectionReportDatesOKBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationCollectionReportDatesOKBody) UnmarshalBinary(b []byte) error {
	var res StationCollectionReportDatesOKBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
