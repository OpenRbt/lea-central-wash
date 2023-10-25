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

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
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
	case 401:
		result := NewStationCollectionReportDatesUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewStationCollectionReportDatesForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
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
		return nil, runtime.NewAPIError("[POST /station-collection-report-dates] stationCollectionReportDates", response, response.Code())
	}
}

// NewStationCollectionReportDatesOK creates a StationCollectionReportDatesOK with default headers values
func NewStationCollectionReportDatesOK() *StationCollectionReportDatesOK {
	return &StationCollectionReportDatesOK{}
}

/*
StationCollectionReportDatesOK describes a response with status code 200, with default header values.

OK
*/
type StationCollectionReportDatesOK struct {
	Payload *StationCollectionReportDatesOKBody
}

// IsSuccess returns true when this station collection report dates o k response has a 2xx status code
func (o *StationCollectionReportDatesOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station collection report dates o k response has a 3xx status code
func (o *StationCollectionReportDatesOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station collection report dates o k response has a 4xx status code
func (o *StationCollectionReportDatesOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station collection report dates o k response has a 5xx status code
func (o *StationCollectionReportDatesOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station collection report dates o k response a status code equal to that given
func (o *StationCollectionReportDatesOK) IsCode(code int) bool {
	return code == 200
}

// Code gets the status code for the station collection report dates o k response
func (o *StationCollectionReportDatesOK) Code() int {
	return 200
}

func (o *StationCollectionReportDatesOK) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesOK  %+v", 200, o.Payload)
}

func (o *StationCollectionReportDatesOK) String() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesOK  %+v", 200, o.Payload)
}

func (o *StationCollectionReportDatesOK) GetPayload() *StationCollectionReportDatesOKBody {
	return o.Payload
}

func (o *StationCollectionReportDatesOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(StationCollectionReportDatesOKBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationCollectionReportDatesUnauthorized creates a StationCollectionReportDatesUnauthorized with default headers values
func NewStationCollectionReportDatesUnauthorized() *StationCollectionReportDatesUnauthorized {
	return &StationCollectionReportDatesUnauthorized{}
}

/*
StationCollectionReportDatesUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type StationCollectionReportDatesUnauthorized struct {
}

// IsSuccess returns true when this station collection report dates unauthorized response has a 2xx status code
func (o *StationCollectionReportDatesUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station collection report dates unauthorized response has a 3xx status code
func (o *StationCollectionReportDatesUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station collection report dates unauthorized response has a 4xx status code
func (o *StationCollectionReportDatesUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this station collection report dates unauthorized response has a 5xx status code
func (o *StationCollectionReportDatesUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this station collection report dates unauthorized response a status code equal to that given
func (o *StationCollectionReportDatesUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the station collection report dates unauthorized response
func (o *StationCollectionReportDatesUnauthorized) Code() int {
	return 401
}

func (o *StationCollectionReportDatesUnauthorized) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesUnauthorized ", 401)
}

func (o *StationCollectionReportDatesUnauthorized) String() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesUnauthorized ", 401)
}

func (o *StationCollectionReportDatesUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationCollectionReportDatesForbidden creates a StationCollectionReportDatesForbidden with default headers values
func NewStationCollectionReportDatesForbidden() *StationCollectionReportDatesForbidden {
	return &StationCollectionReportDatesForbidden{}
}

/*
StationCollectionReportDatesForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type StationCollectionReportDatesForbidden struct {
}

// IsSuccess returns true when this station collection report dates forbidden response has a 2xx status code
func (o *StationCollectionReportDatesForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station collection report dates forbidden response has a 3xx status code
func (o *StationCollectionReportDatesForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station collection report dates forbidden response has a 4xx status code
func (o *StationCollectionReportDatesForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this station collection report dates forbidden response has a 5xx status code
func (o *StationCollectionReportDatesForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this station collection report dates forbidden response a status code equal to that given
func (o *StationCollectionReportDatesForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the station collection report dates forbidden response
func (o *StationCollectionReportDatesForbidden) Code() int {
	return 403
}

func (o *StationCollectionReportDatesForbidden) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesForbidden ", 403)
}

func (o *StationCollectionReportDatesForbidden) String() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesForbidden ", 403)
}

func (o *StationCollectionReportDatesForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationCollectionReportDatesNotFound creates a StationCollectionReportDatesNotFound with default headers values
func NewStationCollectionReportDatesNotFound() *StationCollectionReportDatesNotFound {
	return &StationCollectionReportDatesNotFound{}
}

/*
StationCollectionReportDatesNotFound describes a response with status code 404, with default header values.

not found
*/
type StationCollectionReportDatesNotFound struct {
}

// IsSuccess returns true when this station collection report dates not found response has a 2xx status code
func (o *StationCollectionReportDatesNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station collection report dates not found response has a 3xx status code
func (o *StationCollectionReportDatesNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station collection report dates not found response has a 4xx status code
func (o *StationCollectionReportDatesNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this station collection report dates not found response has a 5xx status code
func (o *StationCollectionReportDatesNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this station collection report dates not found response a status code equal to that given
func (o *StationCollectionReportDatesNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the station collection report dates not found response
func (o *StationCollectionReportDatesNotFound) Code() int {
	return 404
}

func (o *StationCollectionReportDatesNotFound) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesNotFound ", 404)
}

func (o *StationCollectionReportDatesNotFound) String() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesNotFound ", 404)
}

func (o *StationCollectionReportDatesNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationCollectionReportDatesInternalServerError creates a StationCollectionReportDatesInternalServerError with default headers values
func NewStationCollectionReportDatesInternalServerError() *StationCollectionReportDatesInternalServerError {
	return &StationCollectionReportDatesInternalServerError{}
}

/*
StationCollectionReportDatesInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationCollectionReportDatesInternalServerError struct {
}

// IsSuccess returns true when this station collection report dates internal server error response has a 2xx status code
func (o *StationCollectionReportDatesInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station collection report dates internal server error response has a 3xx status code
func (o *StationCollectionReportDatesInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station collection report dates internal server error response has a 4xx status code
func (o *StationCollectionReportDatesInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station collection report dates internal server error response has a 5xx status code
func (o *StationCollectionReportDatesInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station collection report dates internal server error response a status code equal to that given
func (o *StationCollectionReportDatesInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the station collection report dates internal server error response
func (o *StationCollectionReportDatesInternalServerError) Code() int {
	return 500
}

func (o *StationCollectionReportDatesInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesInternalServerError ", 500)
}

func (o *StationCollectionReportDatesInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-collection-report-dates][%d] stationCollectionReportDatesInternalServerError ", 500)
}

func (o *StationCollectionReportDatesInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
StationCollectionReportDatesBody ArgCollectionReportDates
swagger:model StationCollectionReportDatesBody
*/
type StationCollectionReportDatesBody struct {

	// Unix time
	EndDate *int64 `json:"endDate,omitempty"`

	// Unix time
	StartDate *int64 `json:"startDate,omitempty"`

	// station ID
	StationID int64 `json:"stationID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationCollectionReportDatesBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// Unix time
		EndDate *int64 `json:"endDate,omitempty"`

		// Unix time
		StartDate *int64 `json:"startDate,omitempty"`

		// station ID
		StationID int64 `json:"stationID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.EndDate = props.EndDate
	o.StartDate = props.StartDate
	o.StationID = props.StationID
	return nil
}

// Validate validates this station collection report dates body
func (o *StationCollectionReportDatesBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this station collection report dates body based on context it is used
func (o *StationCollectionReportDatesBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
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

/*
StationCollectionReportDatesOKBody ResponseStationCollectionReportDates
swagger:model StationCollectionReportDatesOKBody
*/
type StationCollectionReportDatesOKBody struct {

	// collection reports
	// Required: true
	CollectionReports []*model.CollectionReportWithUser `json:"collectionReports"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationCollectionReportDatesOKBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// collection reports
		// Required: true
		CollectionReports []*model.CollectionReportWithUser `json:"collectionReports"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.CollectionReports = props.CollectionReports
	return nil
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
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationCollectionReportDatesOK" + "." + "collectionReports" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// ContextValidate validate this station collection report dates o k body based on the context it is used
func (o *StationCollectionReportDatesOKBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateCollectionReports(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationCollectionReportDatesOKBody) contextValidateCollectionReports(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(o.CollectionReports); i++ {

		if o.CollectionReports[i] != nil {

			if swag.IsZero(o.CollectionReports[i]) { // not required
				return nil
			}

			if err := o.CollectionReports[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("stationCollectionReportDatesOK" + "." + "collectionReports" + "." + strconv.Itoa(i))
				} else if ce, ok := err.(*errors.CompositeError); ok {
					return ce.ValidateName("stationCollectionReportDatesOK" + "." + "collectionReports" + "." + strconv.Itoa(i))
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
