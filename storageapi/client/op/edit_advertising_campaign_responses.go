// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
)

// EditAdvertisingCampaignReader is a Reader for the EditAdvertisingCampaign structure.
type EditAdvertisingCampaignReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *EditAdvertisingCampaignReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewEditAdvertisingCampaignNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewEditAdvertisingCampaignUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewEditAdvertisingCampaignForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewEditAdvertisingCampaignInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /edit-advertising-campaign] editAdvertisingCampaign", response, response.Code())
	}
}

// NewEditAdvertisingCampaignNoContent creates a EditAdvertisingCampaignNoContent with default headers values
func NewEditAdvertisingCampaignNoContent() *EditAdvertisingCampaignNoContent {
	return &EditAdvertisingCampaignNoContent{}
}

/* EditAdvertisingCampaignNoContent describes a response with status code 204, with default header values.

OK
*/
type EditAdvertisingCampaignNoContent struct {
}

// IsSuccess returns true when this edit advertising campaign no content response has a 2xx status code
func (o *EditAdvertisingCampaignNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this edit advertising campaign no content response has a 3xx status code
func (o *EditAdvertisingCampaignNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this edit advertising campaign no content response has a 4xx status code
func (o *EditAdvertisingCampaignNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this edit advertising campaign no content response has a 5xx status code
func (o *EditAdvertisingCampaignNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this edit advertising campaign no content response a status code equal to that given
func (o *EditAdvertisingCampaignNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the edit advertising campaign no content response
func (o *EditAdvertisingCampaignNoContent) Code() int {
	return 204
}

func (o *EditAdvertisingCampaignNoContent) Error() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignNoContent ", 204)
}

func (o *EditAdvertisingCampaignNoContent) String() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignNoContent ", 204)
}

func (o *EditAdvertisingCampaignNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEditAdvertisingCampaignUnauthorized creates a EditAdvertisingCampaignUnauthorized with default headers values
func NewEditAdvertisingCampaignUnauthorized() *EditAdvertisingCampaignUnauthorized {
	return &EditAdvertisingCampaignUnauthorized{}
}

/* EditAdvertisingCampaignUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type EditAdvertisingCampaignUnauthorized struct {
}

// IsSuccess returns true when this edit advertising campaign unauthorized response has a 2xx status code
func (o *EditAdvertisingCampaignUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this edit advertising campaign unauthorized response has a 3xx status code
func (o *EditAdvertisingCampaignUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this edit advertising campaign unauthorized response has a 4xx status code
func (o *EditAdvertisingCampaignUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this edit advertising campaign unauthorized response has a 5xx status code
func (o *EditAdvertisingCampaignUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this edit advertising campaign unauthorized response a status code equal to that given
func (o *EditAdvertisingCampaignUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the edit advertising campaign unauthorized response
func (o *EditAdvertisingCampaignUnauthorized) Code() int {
	return 401
}

func (o *EditAdvertisingCampaignUnauthorized) Error() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignUnauthorized ", 401)
}

func (o *EditAdvertisingCampaignUnauthorized) String() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignUnauthorized ", 401)
}

func (o *EditAdvertisingCampaignUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEditAdvertisingCampaignForbidden creates a EditAdvertisingCampaignForbidden with default headers values
func NewEditAdvertisingCampaignForbidden() *EditAdvertisingCampaignForbidden {
	return &EditAdvertisingCampaignForbidden{}
}

/* EditAdvertisingCampaignForbidden describes a response with status code 403, with default header values.

Access forbiddenn
*/
type EditAdvertisingCampaignForbidden struct {
}

// IsSuccess returns true when this edit advertising campaign forbidden response has a 2xx status code
func (o *EditAdvertisingCampaignForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this edit advertising campaign forbidden response has a 3xx status code
func (o *EditAdvertisingCampaignForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this edit advertising campaign forbidden response has a 4xx status code
func (o *EditAdvertisingCampaignForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this edit advertising campaign forbidden response has a 5xx status code
func (o *EditAdvertisingCampaignForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this edit advertising campaign forbidden response a status code equal to that given
func (o *EditAdvertisingCampaignForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the edit advertising campaign forbidden response
func (o *EditAdvertisingCampaignForbidden) Code() int {
	return 403
}

func (o *EditAdvertisingCampaignForbidden) Error() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignForbidden ", 403)
}

func (o *EditAdvertisingCampaignForbidden) String() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignForbidden ", 403)
}

func (o *EditAdvertisingCampaignForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewEditAdvertisingCampaignInternalServerError creates a EditAdvertisingCampaignInternalServerError with default headers values
func NewEditAdvertisingCampaignInternalServerError() *EditAdvertisingCampaignInternalServerError {
	return &EditAdvertisingCampaignInternalServerError{}
}

/* EditAdvertisingCampaignInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type EditAdvertisingCampaignInternalServerError struct {
}

// IsSuccess returns true when this edit advertising campaign internal server error response has a 2xx status code
func (o *EditAdvertisingCampaignInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this edit advertising campaign internal server error response has a 3xx status code
func (o *EditAdvertisingCampaignInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this edit advertising campaign internal server error response has a 4xx status code
func (o *EditAdvertisingCampaignInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this edit advertising campaign internal server error response has a 5xx status code
func (o *EditAdvertisingCampaignInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this edit advertising campaign internal server error response a status code equal to that given
func (o *EditAdvertisingCampaignInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the edit advertising campaign internal server error response
func (o *EditAdvertisingCampaignInternalServerError) Code() int {
	return 500
}

func (o *EditAdvertisingCampaignInternalServerError) Error() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignInternalServerError ", 500)
}

func (o *EditAdvertisingCampaignInternalServerError) String() string {
	return fmt.Sprintf("[POST /edit-advertising-campaign][%d] editAdvertisingCampaignInternalServerError ", 500)
}

func (o *EditAdvertisingCampaignInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
