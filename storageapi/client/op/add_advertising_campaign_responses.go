// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// AddAdvertisingCampaignReader is a Reader for the AddAdvertisingCampaign structure.
type AddAdvertisingCampaignReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *AddAdvertisingCampaignReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewAddAdvertisingCampaignNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewAddAdvertisingCampaignUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewAddAdvertisingCampaignForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewAddAdvertisingCampaignInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewAddAdvertisingCampaignNoContent creates a AddAdvertisingCampaignNoContent with default headers values
func NewAddAdvertisingCampaignNoContent() *AddAdvertisingCampaignNoContent {
	return &AddAdvertisingCampaignNoContent{}
}

/*
AddAdvertisingCampaignNoContent describes a response with status code 204, with default header values.

OK
*/
type AddAdvertisingCampaignNoContent struct {
}

// IsSuccess returns true when this add advertising campaign no content response has a 2xx status code
func (o *AddAdvertisingCampaignNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this add advertising campaign no content response has a 3xx status code
func (o *AddAdvertisingCampaignNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this add advertising campaign no content response has a 4xx status code
func (o *AddAdvertisingCampaignNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this add advertising campaign no content response has a 5xx status code
func (o *AddAdvertisingCampaignNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this add advertising campaign no content response a status code equal to that given
func (o *AddAdvertisingCampaignNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *AddAdvertisingCampaignNoContent) Error() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignNoContent ", 204)
}

func (o *AddAdvertisingCampaignNoContent) String() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignNoContent ", 204)
}

func (o *AddAdvertisingCampaignNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAddAdvertisingCampaignUnauthorized creates a AddAdvertisingCampaignUnauthorized with default headers values
func NewAddAdvertisingCampaignUnauthorized() *AddAdvertisingCampaignUnauthorized {
	return &AddAdvertisingCampaignUnauthorized{}
}

/*
AddAdvertisingCampaignUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type AddAdvertisingCampaignUnauthorized struct {
}

// IsSuccess returns true when this add advertising campaign unauthorized response has a 2xx status code
func (o *AddAdvertisingCampaignUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this add advertising campaign unauthorized response has a 3xx status code
func (o *AddAdvertisingCampaignUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this add advertising campaign unauthorized response has a 4xx status code
func (o *AddAdvertisingCampaignUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this add advertising campaign unauthorized response has a 5xx status code
func (o *AddAdvertisingCampaignUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this add advertising campaign unauthorized response a status code equal to that given
func (o *AddAdvertisingCampaignUnauthorized) IsCode(code int) bool {
	return code == 401
}

func (o *AddAdvertisingCampaignUnauthorized) Error() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignUnauthorized ", 401)
}

func (o *AddAdvertisingCampaignUnauthorized) String() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignUnauthorized ", 401)
}

func (o *AddAdvertisingCampaignUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAddAdvertisingCampaignForbidden creates a AddAdvertisingCampaignForbidden with default headers values
func NewAddAdvertisingCampaignForbidden() *AddAdvertisingCampaignForbidden {
	return &AddAdvertisingCampaignForbidden{}
}

/*
AddAdvertisingCampaignForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type AddAdvertisingCampaignForbidden struct {
}

// IsSuccess returns true when this add advertising campaign forbidden response has a 2xx status code
func (o *AddAdvertisingCampaignForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this add advertising campaign forbidden response has a 3xx status code
func (o *AddAdvertisingCampaignForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this add advertising campaign forbidden response has a 4xx status code
func (o *AddAdvertisingCampaignForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this add advertising campaign forbidden response has a 5xx status code
func (o *AddAdvertisingCampaignForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this add advertising campaign forbidden response a status code equal to that given
func (o *AddAdvertisingCampaignForbidden) IsCode(code int) bool {
	return code == 403
}

func (o *AddAdvertisingCampaignForbidden) Error() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignForbidden ", 403)
}

func (o *AddAdvertisingCampaignForbidden) String() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignForbidden ", 403)
}

func (o *AddAdvertisingCampaignForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewAddAdvertisingCampaignInternalServerError creates a AddAdvertisingCampaignInternalServerError with default headers values
func NewAddAdvertisingCampaignInternalServerError() *AddAdvertisingCampaignInternalServerError {
	return &AddAdvertisingCampaignInternalServerError{}
}

/*
AddAdvertisingCampaignInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type AddAdvertisingCampaignInternalServerError struct {
}

// IsSuccess returns true when this add advertising campaign internal server error response has a 2xx status code
func (o *AddAdvertisingCampaignInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this add advertising campaign internal server error response has a 3xx status code
func (o *AddAdvertisingCampaignInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this add advertising campaign internal server error response has a 4xx status code
func (o *AddAdvertisingCampaignInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this add advertising campaign internal server error response has a 5xx status code
func (o *AddAdvertisingCampaignInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this add advertising campaign internal server error response a status code equal to that given
func (o *AddAdvertisingCampaignInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *AddAdvertisingCampaignInternalServerError) Error() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignInternalServerError ", 500)
}

func (o *AddAdvertisingCampaignInternalServerError) String() string {
	return fmt.Sprintf("[POST /add-advertising-campaign][%d] addAdvertisingCampaignInternalServerError ", 500)
}

func (o *AddAdvertisingCampaignInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
