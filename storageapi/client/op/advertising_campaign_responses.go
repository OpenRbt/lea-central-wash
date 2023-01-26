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

// AdvertisingCampaignReader is a Reader for the AdvertisingCampaign structure.
type AdvertisingCampaignReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *AdvertisingCampaignReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewAdvertisingCampaignOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewAdvertisingCampaignUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewAdvertisingCampaignForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewAdvertisingCampaignInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewAdvertisingCampaignOK creates a AdvertisingCampaignOK with default headers values
func NewAdvertisingCampaignOK() *AdvertisingCampaignOK {
	return &AdvertisingCampaignOK{}
}

/*
AdvertisingCampaignOK describes a response with status code 200, with default header values.

OK
*/
type AdvertisingCampaignOK struct {
	Payload model.AdvertisingCampaigns
}

// IsSuccess returns true when this advertising campaign o k response has a 2xx status code
func (o *AdvertisingCampaignOK) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this advertising campaign o k response has a 3xx status code
func (o *AdvertisingCampaignOK) IsRedirect() bool {
	return false
}

// IsClientError returns true when this advertising campaign o k response has a 4xx status code
func (o *AdvertisingCampaignOK) IsClientError() bool {
	return false
}

// IsServerError returns true when this advertising campaign o k response has a 5xx status code
func (o *AdvertisingCampaignOK) IsServerError() bool {
	return false
}

// IsCode returns true when this advertising campaign o k response a status code equal to that given
func (o *AdvertisingCampaignOK) IsCode(code int) bool {
	return code == 200
}

func (o *AdvertisingCampaignOK) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignOK  %+v", 200, o.Payload)
}

func (o *AdvertisingCampaignOK) String() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignOK  %+v", 200, o.Payload)
}

func (o *AdvertisingCampaignOK) GetPayload() model.AdvertisingCampaigns {
	return o.Payload
}

func (o *AdvertisingCampaignOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewAdvertisingCampaignUnauthorized creates a AdvertisingCampaignUnauthorized with default headers values
func NewAdvertisingCampaignUnauthorized() *AdvertisingCampaignUnauthorized {
	return &AdvertisingCampaignUnauthorized{}
}

/*
AdvertisingCampaignUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type AdvertisingCampaignUnauthorized struct {
}

// IsSuccess returns true when this advertising campaign unauthorized response has a 2xx status code
func (o *AdvertisingCampaignUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this advertising campaign unauthorized response has a 3xx status code
func (o *AdvertisingCampaignUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this advertising campaign unauthorized response has a 4xx status code
func (o *AdvertisingCampaignUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this advertising campaign unauthorized response has a 5xx status code
func (o *AdvertisingCampaignUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this advertising campaign unauthorized response a status code equal to that given
func (o *AdvertisingCampaignUnauthorized) IsCode(code int) bool {
	return code == 401
}

func (o *AdvertisingCampaignUnauthorized) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignUnauthorized ", 401)
}

func (o *AdvertisingCampaignUnauthorized) String() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignUnauthorized ", 401)
}

func (o *AdvertisingCampaignUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAdvertisingCampaignForbidden creates a AdvertisingCampaignForbidden with default headers values
func NewAdvertisingCampaignForbidden() *AdvertisingCampaignForbidden {
	return &AdvertisingCampaignForbidden{}
}

/*
AdvertisingCampaignForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type AdvertisingCampaignForbidden struct {
}

// IsSuccess returns true when this advertising campaign forbidden response has a 2xx status code
func (o *AdvertisingCampaignForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this advertising campaign forbidden response has a 3xx status code
func (o *AdvertisingCampaignForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this advertising campaign forbidden response has a 4xx status code
func (o *AdvertisingCampaignForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this advertising campaign forbidden response has a 5xx status code
func (o *AdvertisingCampaignForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this advertising campaign forbidden response a status code equal to that given
func (o *AdvertisingCampaignForbidden) IsCode(code int) bool {
	return code == 403
}

func (o *AdvertisingCampaignForbidden) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignForbidden ", 403)
}

func (o *AdvertisingCampaignForbidden) String() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignForbidden ", 403)
}

func (o *AdvertisingCampaignForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAdvertisingCampaignInternalServerError creates a AdvertisingCampaignInternalServerError with default headers values
func NewAdvertisingCampaignInternalServerError() *AdvertisingCampaignInternalServerError {
	return &AdvertisingCampaignInternalServerError{}
}

/*
AdvertisingCampaignInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type AdvertisingCampaignInternalServerError struct {
}

// IsSuccess returns true when this advertising campaign internal server error response has a 2xx status code
func (o *AdvertisingCampaignInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this advertising campaign internal server error response has a 3xx status code
func (o *AdvertisingCampaignInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this advertising campaign internal server error response has a 4xx status code
func (o *AdvertisingCampaignInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this advertising campaign internal server error response has a 5xx status code
func (o *AdvertisingCampaignInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this advertising campaign internal server error response a status code equal to that given
func (o *AdvertisingCampaignInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *AdvertisingCampaignInternalServerError) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignInternalServerError ", 500)
}

func (o *AdvertisingCampaignInternalServerError) String() string {
	return fmt.Sprintf("[POST /advertising-campaign][%d] advertisingCampaignInternalServerError ", 500)
}

func (o *AdvertisingCampaignInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
AdvertisingCampaignBody ArgAdvertisingCampagin
swagger:model AdvertisingCampaignBody
*/
type AdvertisingCampaignBody struct {

	// Unix time local
	EndDate *int64 `json:"endDate,omitempty"`

	// Unix time local
	StartDate *int64 `json:"startDate,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AdvertisingCampaignBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// Unix time local
		EndDate *int64 `json:"endDate,omitempty"`

		// Unix time local
		StartDate *int64 `json:"startDate,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.EndDate = props.EndDate
	o.StartDate = props.StartDate
	return nil
}

// Validate validates this advertising campaign body
func (o *AdvertisingCampaignBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this advertising campaign body based on context it is used
func (o *AdvertisingCampaignBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AdvertisingCampaignBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AdvertisingCampaignBody) UnmarshalBinary(b []byte) error {
	var res AdvertisingCampaignBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
