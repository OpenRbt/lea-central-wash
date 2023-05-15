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

// DelAdvertisingCampaignReader is a Reader for the DelAdvertisingCampaign structure.
type DelAdvertisingCampaignReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *DelAdvertisingCampaignReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewDelAdvertisingCampaignNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewDelAdvertisingCampaignUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewDelAdvertisingCampaignForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewDelAdvertisingCampaignInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewDelAdvertisingCampaignNoContent creates a DelAdvertisingCampaignNoContent with default headers values
func NewDelAdvertisingCampaignNoContent() *DelAdvertisingCampaignNoContent {
	return &DelAdvertisingCampaignNoContent{}
}

/*
DelAdvertisingCampaignNoContent describes a response with status code 204, with default header values.

OK
*/
type DelAdvertisingCampaignNoContent struct {
}

// IsSuccess returns true when this del advertising campaign no content response has a 2xx status code
func (o *DelAdvertisingCampaignNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this del advertising campaign no content response has a 3xx status code
func (o *DelAdvertisingCampaignNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del advertising campaign no content response has a 4xx status code
func (o *DelAdvertisingCampaignNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this del advertising campaign no content response has a 5xx status code
func (o *DelAdvertisingCampaignNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this del advertising campaign no content response a status code equal to that given
func (o *DelAdvertisingCampaignNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the del advertising campaign no content response
func (o *DelAdvertisingCampaignNoContent) Code() int {
	return 204
}

func (o *DelAdvertisingCampaignNoContent) Error() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignNoContent ", 204)
}

func (o *DelAdvertisingCampaignNoContent) String() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignNoContent ", 204)
}

func (o *DelAdvertisingCampaignNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDelAdvertisingCampaignUnauthorized creates a DelAdvertisingCampaignUnauthorized with default headers values
func NewDelAdvertisingCampaignUnauthorized() *DelAdvertisingCampaignUnauthorized {
	return &DelAdvertisingCampaignUnauthorized{}
}

/*
DelAdvertisingCampaignUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type DelAdvertisingCampaignUnauthorized struct {
}

// IsSuccess returns true when this del advertising campaign unauthorized response has a 2xx status code
func (o *DelAdvertisingCampaignUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this del advertising campaign unauthorized response has a 3xx status code
func (o *DelAdvertisingCampaignUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del advertising campaign unauthorized response has a 4xx status code
func (o *DelAdvertisingCampaignUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this del advertising campaign unauthorized response has a 5xx status code
func (o *DelAdvertisingCampaignUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this del advertising campaign unauthorized response a status code equal to that given
func (o *DelAdvertisingCampaignUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the del advertising campaign unauthorized response
func (o *DelAdvertisingCampaignUnauthorized) Code() int {
	return 401
}

func (o *DelAdvertisingCampaignUnauthorized) Error() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignUnauthorized ", 401)
}

func (o *DelAdvertisingCampaignUnauthorized) String() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignUnauthorized ", 401)
}

func (o *DelAdvertisingCampaignUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDelAdvertisingCampaignForbidden creates a DelAdvertisingCampaignForbidden with default headers values
func NewDelAdvertisingCampaignForbidden() *DelAdvertisingCampaignForbidden {
	return &DelAdvertisingCampaignForbidden{}
}

/*
DelAdvertisingCampaignForbidden describes a response with status code 403, with default header values.

Access forbidden
*/
type DelAdvertisingCampaignForbidden struct {
}

// IsSuccess returns true when this del advertising campaign forbidden response has a 2xx status code
func (o *DelAdvertisingCampaignForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this del advertising campaign forbidden response has a 3xx status code
func (o *DelAdvertisingCampaignForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del advertising campaign forbidden response has a 4xx status code
func (o *DelAdvertisingCampaignForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this del advertising campaign forbidden response has a 5xx status code
func (o *DelAdvertisingCampaignForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this del advertising campaign forbidden response a status code equal to that given
func (o *DelAdvertisingCampaignForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the del advertising campaign forbidden response
func (o *DelAdvertisingCampaignForbidden) Code() int {
	return 403
}

func (o *DelAdvertisingCampaignForbidden) Error() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignForbidden ", 403)
}

func (o *DelAdvertisingCampaignForbidden) String() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignForbidden ", 403)
}

func (o *DelAdvertisingCampaignForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewDelAdvertisingCampaignInternalServerError creates a DelAdvertisingCampaignInternalServerError with default headers values
func NewDelAdvertisingCampaignInternalServerError() *DelAdvertisingCampaignInternalServerError {
	return &DelAdvertisingCampaignInternalServerError{}
}

/*
DelAdvertisingCampaignInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type DelAdvertisingCampaignInternalServerError struct {
}

// IsSuccess returns true when this del advertising campaign internal server error response has a 2xx status code
func (o *DelAdvertisingCampaignInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this del advertising campaign internal server error response has a 3xx status code
func (o *DelAdvertisingCampaignInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this del advertising campaign internal server error response has a 4xx status code
func (o *DelAdvertisingCampaignInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this del advertising campaign internal server error response has a 5xx status code
func (o *DelAdvertisingCampaignInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this del advertising campaign internal server error response a status code equal to that given
func (o *DelAdvertisingCampaignInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the del advertising campaign internal server error response
func (o *DelAdvertisingCampaignInternalServerError) Code() int {
	return 500
}

func (o *DelAdvertisingCampaignInternalServerError) Error() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignInternalServerError ", 500)
}

func (o *DelAdvertisingCampaignInternalServerError) String() string {
	return fmt.Sprintf("[POST /del-advertising-campaign][%d] delAdvertisingCampaignInternalServerError ", 500)
}

func (o *DelAdvertisingCampaignInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
DelAdvertisingCampaignBody ArgDelAdvertisingCampagin
swagger:model DelAdvertisingCampaignBody
*/
type DelAdvertisingCampaignBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DelAdvertisingCampaignBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// id
		// Required: true
		ID *int64 `json:"id"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ID = props.ID
	return nil
}

// Validate validates this del advertising campaign body
func (o *DelAdvertisingCampaignBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DelAdvertisingCampaignBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this del advertising campaign body based on context it is used
func (o *DelAdvertisingCampaignBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *DelAdvertisingCampaignBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DelAdvertisingCampaignBody) UnmarshalBinary(b []byte) error {
	var res DelAdvertisingCampaignBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
