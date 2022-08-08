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

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationStatCurrentReader is a Reader for the StationStatCurrent structure.
type StationStatCurrentReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationStatCurrentReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewStationStatCurrentOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewStationStatCurrentUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewStationStatCurrentForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewStationStatCurrentInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewStationStatCurrentOK creates a StationStatCurrentOK with default headers values
func NewStationStatCurrentOK() *StationStatCurrentOK {
	return &StationStatCurrentOK{}
}

/* StationStatCurrentOK describes a response with status code 200, with default header values.

OK
*/
type StationStatCurrentOK struct {
	Payload model.StationsStat
}

// IsSuccess returns true when this station stat current o k response has a 2xx status code
func (o *StationStatCurrentOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this station stat current o k response has a 3xx status code
func (o *StationStatCurrentOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station stat current o k response has a 4xx status code
func (o *StationStatCurrentOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this station stat current o k response has a 5xx status code
func (o *StationStatCurrentOK) IsServerError() bool {
	return false
}

// IsCode returns true when this station stat current o k response a status code equal to that given
func (o *StationStatCurrentOK) IsCode(code int) bool {
	return code == 200
}

func (o *StationStatCurrentOK) Error() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentOK  %+v", 200, o.Payload)
}

func (o *StationStatCurrentOK) String() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentOK  %+v", 200, o.Payload)
}

func (o *StationStatCurrentOK) GetPayload() model.StationsStat {
	return o.Payload
}

func (o *StationStatCurrentOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationStatCurrentUnauthorized creates a StationStatCurrentUnauthorized with default headers values
func NewStationStatCurrentUnauthorized() *StationStatCurrentUnauthorized {
	return &StationStatCurrentUnauthorized{}
}

/* StationStatCurrentUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type StationStatCurrentUnauthorized struct {
}

// IsSuccess returns true when this station stat current unauthorized response has a 2xx status code
func (o *StationStatCurrentUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station stat current unauthorized response has a 3xx status code
func (o *StationStatCurrentUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station stat current unauthorized response has a 4xx status code
func (o *StationStatCurrentUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this station stat current unauthorized response has a 5xx status code
func (o *StationStatCurrentUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this station stat current unauthorized response a status code equal to that given
func (o *StationStatCurrentUnauthorized) IsCode(code int) bool {
	return code == 401
}

func (o *StationStatCurrentUnauthorized) Error() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentUnauthorized ", 401)
}

func (o *StationStatCurrentUnauthorized) String() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentUnauthorized ", 401)
}

func (o *StationStatCurrentUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationStatCurrentForbidden creates a StationStatCurrentForbidden with default headers values
func NewStationStatCurrentForbidden() *StationStatCurrentForbidden {
	return &StationStatCurrentForbidden{}
}

/* StationStatCurrentForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type StationStatCurrentForbidden struct {
}

// IsSuccess returns true when this station stat current forbidden response has a 2xx status code
func (o *StationStatCurrentForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station stat current forbidden response has a 3xx status code
func (o *StationStatCurrentForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station stat current forbidden response has a 4xx status code
func (o *StationStatCurrentForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this station stat current forbidden response has a 5xx status code
func (o *StationStatCurrentForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this station stat current forbidden response a status code equal to that given
func (o *StationStatCurrentForbidden) IsCode(code int) bool {
	return code == 403
}

func (o *StationStatCurrentForbidden) Error() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentForbidden ", 403)
}

func (o *StationStatCurrentForbidden) String() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentForbidden ", 403)
}

func (o *StationStatCurrentForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewStationStatCurrentInternalServerError creates a StationStatCurrentInternalServerError with default headers values
func NewStationStatCurrentInternalServerError() *StationStatCurrentInternalServerError {
	return &StationStatCurrentInternalServerError{}
}

/* StationStatCurrentInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type StationStatCurrentInternalServerError struct {
}

// IsSuccess returns true when this station stat current internal server error response has a 2xx status code
func (o *StationStatCurrentInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this station stat current internal server error response has a 3xx status code
func (o *StationStatCurrentInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this station stat current internal server error response has a 4xx status code
func (o *StationStatCurrentInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this station stat current internal server error response has a 5xx status code
func (o *StationStatCurrentInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this station stat current internal server error response a status code equal to that given
func (o *StationStatCurrentInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *StationStatCurrentInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentInternalServerError ", 500)
}

func (o *StationStatCurrentInternalServerError) String() string {
	return fmt.Sprintf("[POST /station-stat-current][%d] stationStatCurrentInternalServerError ", 500)
}

func (o *StationStatCurrentInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*StationStatCurrentBody ArgStationStat
swagger:model StationStatCurrentBody
*/
type StationStatCurrentBody struct {

	// station ID
	StationID *int64 `json:"stationID,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *StationStatCurrentBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// station ID
		StationID *int64 `json:"stationID,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.StationID = props.StationID
	return nil
}

// Validate validates this station stat current body
func (o *StationStatCurrentBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this station stat current body based on context it is used
func (o *StationStatCurrentBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *StationStatCurrentBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationStatCurrentBody) UnmarshalBinary(b []byte) error {
	var res StationStatCurrentBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
