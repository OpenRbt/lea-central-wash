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

// AdvertisingCampaignByIDReader is a Reader for the AdvertisingCampaignByID structure.
type AdvertisingCampaignByIDReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *AdvertisingCampaignByIDReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 200:
		result := NewAdvertisingCampaignByIDOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewAdvertisingCampaignByIDUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewAdvertisingCampaignByIDForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 404:
		result := NewAdvertisingCampaignByIDNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewAdvertisingCampaignByIDInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewAdvertisingCampaignByIDOK creates a AdvertisingCampaignByIDOK with default headers values
func NewAdvertisingCampaignByIDOK() *AdvertisingCampaignByIDOK {
	return &AdvertisingCampaignByIDOK{}
}

/* AdvertisingCampaignByIDOK describes a response with status code 200, with default header values.

OK
*/
type AdvertisingCampaignByIDOK struct {
	Payload *model.AdvertisingCampaign
}

func (o *AdvertisingCampaignByIDOK) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign-by-id][%d] advertisingCampaignByIdOK  %+v", 200, o.Payload)
}
func (o *AdvertisingCampaignByIDOK) GetPayload() *model.AdvertisingCampaign {
	return o.Payload
}

func (o *AdvertisingCampaignByIDOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.AdvertisingCampaign)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewAdvertisingCampaignByIDUnauthorized creates a AdvertisingCampaignByIDUnauthorized with default headers values
func NewAdvertisingCampaignByIDUnauthorized() *AdvertisingCampaignByIDUnauthorized {
	return &AdvertisingCampaignByIDUnauthorized{}
}

/* AdvertisingCampaignByIDUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type AdvertisingCampaignByIDUnauthorized struct {
}

func (o *AdvertisingCampaignByIDUnauthorized) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign-by-id][%d] advertisingCampaignByIdUnauthorized ", 401)
}

func (o *AdvertisingCampaignByIDUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAdvertisingCampaignByIDForbidden creates a AdvertisingCampaignByIDForbidden with default headers values
func NewAdvertisingCampaignByIDForbidden() *AdvertisingCampaignByIDForbidden {
	return &AdvertisingCampaignByIDForbidden{}
}

/* AdvertisingCampaignByIDForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type AdvertisingCampaignByIDForbidden struct {
}

func (o *AdvertisingCampaignByIDForbidden) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign-by-id][%d] advertisingCampaignByIdForbidden ", 403)
}

func (o *AdvertisingCampaignByIDForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAdvertisingCampaignByIDNotFound creates a AdvertisingCampaignByIDNotFound with default headers values
func NewAdvertisingCampaignByIDNotFound() *AdvertisingCampaignByIDNotFound {
	return &AdvertisingCampaignByIDNotFound{}
}

/* AdvertisingCampaignByIDNotFound describes a response with status code 404, with default header values.

not found
*/
type AdvertisingCampaignByIDNotFound struct {
}

func (o *AdvertisingCampaignByIDNotFound) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign-by-id][%d] advertisingCampaignByIdNotFound ", 404)
}

func (o *AdvertisingCampaignByIDNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAdvertisingCampaignByIDInternalServerError creates a AdvertisingCampaignByIDInternalServerError with default headers values
func NewAdvertisingCampaignByIDInternalServerError() *AdvertisingCampaignByIDInternalServerError {
	return &AdvertisingCampaignByIDInternalServerError{}
}

/* AdvertisingCampaignByIDInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type AdvertisingCampaignByIDInternalServerError struct {
}

func (o *AdvertisingCampaignByIDInternalServerError) Error() string {
	return fmt.Sprintf("[POST /advertising-campaign-by-id][%d] advertisingCampaignByIdInternalServerError ", 500)
}

func (o *AdvertisingCampaignByIDInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*AdvertisingCampaignByIDBody ArgAdvertisingCampaignByID
swagger:model AdvertisingCampaignByIDBody
*/
type AdvertisingCampaignByIDBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AdvertisingCampaignByIDBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this advertising campaign by ID body
func (o *AdvertisingCampaignByIDBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *AdvertisingCampaignByIDBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this advertising campaign by ID body based on context it is used
func (o *AdvertisingCampaignByIDBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AdvertisingCampaignByIDBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AdvertisingCampaignByIDBody) UnmarshalBinary(b []byte) error {
	var res AdvertisingCampaignByIDBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
