// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// DelAdvertisingCampaignNoContentCode is the HTTP code returned for type DelAdvertisingCampaignNoContent
const DelAdvertisingCampaignNoContentCode int = 204

/*
DelAdvertisingCampaignNoContent OK

swagger:response delAdvertisingCampaignNoContent
*/
type DelAdvertisingCampaignNoContent struct {
}

// NewDelAdvertisingCampaignNoContent creates DelAdvertisingCampaignNoContent with default headers values
func NewDelAdvertisingCampaignNoContent() *DelAdvertisingCampaignNoContent {

	return &DelAdvertisingCampaignNoContent{}
}

// WriteResponse to the client
func (o *DelAdvertisingCampaignNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *DelAdvertisingCampaignNoContent) DelAdvertisingCampaignResponder() {}

// DelAdvertisingCampaignUnauthorizedCode is the HTTP code returned for type DelAdvertisingCampaignUnauthorized
const DelAdvertisingCampaignUnauthorizedCode int = 401

/*
DelAdvertisingCampaignUnauthorized PIN is missing or invalid

swagger:response delAdvertisingCampaignUnauthorized
*/
type DelAdvertisingCampaignUnauthorized struct {
}

// NewDelAdvertisingCampaignUnauthorized creates DelAdvertisingCampaignUnauthorized with default headers values
func NewDelAdvertisingCampaignUnauthorized() *DelAdvertisingCampaignUnauthorized {

	return &DelAdvertisingCampaignUnauthorized{}
}

// WriteResponse to the client
func (o *DelAdvertisingCampaignUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *DelAdvertisingCampaignUnauthorized) DelAdvertisingCampaignResponder() {}

// DelAdvertisingCampaignForbiddenCode is the HTTP code returned for type DelAdvertisingCampaignForbidden
const DelAdvertisingCampaignForbiddenCode int = 403

/*
DelAdvertisingCampaignForbidden Access forbiddenn

swagger:response delAdvertisingCampaignForbidden
*/
type DelAdvertisingCampaignForbidden struct {
}

// NewDelAdvertisingCampaignForbidden creates DelAdvertisingCampaignForbidden with default headers values
func NewDelAdvertisingCampaignForbidden() *DelAdvertisingCampaignForbidden {

	return &DelAdvertisingCampaignForbidden{}
}

// WriteResponse to the client
func (o *DelAdvertisingCampaignForbidden) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(403)
}

func (o *DelAdvertisingCampaignForbidden) DelAdvertisingCampaignResponder() {}

// DelAdvertisingCampaignInternalServerErrorCode is the HTTP code returned for type DelAdvertisingCampaignInternalServerError
const DelAdvertisingCampaignInternalServerErrorCode int = 500

/*
DelAdvertisingCampaignInternalServerError internal error

swagger:response delAdvertisingCampaignInternalServerError
*/
type DelAdvertisingCampaignInternalServerError struct {
}

// NewDelAdvertisingCampaignInternalServerError creates DelAdvertisingCampaignInternalServerError with default headers values
func NewDelAdvertisingCampaignInternalServerError() *DelAdvertisingCampaignInternalServerError {

	return &DelAdvertisingCampaignInternalServerError{}
}

// WriteResponse to the client
func (o *DelAdvertisingCampaignInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *DelAdvertisingCampaignInternalServerError) DelAdvertisingCampaignResponder() {}

type DelAdvertisingCampaignNotImplementedResponder struct {
	middleware.Responder
}

func (*DelAdvertisingCampaignNotImplementedResponder) DelAdvertisingCampaignResponder() {}

func DelAdvertisingCampaignNotImplemented() DelAdvertisingCampaignResponder {
	return &DelAdvertisingCampaignNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.DelAdvertisingCampaign has not yet been implemented",
		),
	}
}

type DelAdvertisingCampaignResponder interface {
	middleware.Responder
	DelAdvertisingCampaignResponder()
}
