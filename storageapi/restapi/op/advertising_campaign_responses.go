// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// AdvertisingCampaignOKCode is the HTTP code returned for type AdvertisingCampaignOK
const AdvertisingCampaignOKCode int = 200

/*
AdvertisingCampaignOK OK

swagger:response advertisingCampaignOK
*/
type AdvertisingCampaignOK struct {

	/*
	  In: Body
	*/
	Payload model.AdvertisingCampaigns `json:"body,omitempty"`
}

// NewAdvertisingCampaignOK creates AdvertisingCampaignOK with default headers values
func NewAdvertisingCampaignOK() *AdvertisingCampaignOK {

	return &AdvertisingCampaignOK{}
}

// WithPayload adds the payload to the advertising campaign o k response
func (o *AdvertisingCampaignOK) WithPayload(payload model.AdvertisingCampaigns) *AdvertisingCampaignOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the advertising campaign o k response
func (o *AdvertisingCampaignOK) SetPayload(payload model.AdvertisingCampaigns) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *AdvertisingCampaignOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if payload == nil {
		// return empty array
		payload = model.AdvertisingCampaigns{}
	}

	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *AdvertisingCampaignOK) AdvertisingCampaignResponder() {}

// AdvertisingCampaignUnauthorizedCode is the HTTP code returned for type AdvertisingCampaignUnauthorized
const AdvertisingCampaignUnauthorizedCode int = 401

/*
AdvertisingCampaignUnauthorized PIN is missing or invalid

swagger:response advertisingCampaignUnauthorized
*/
type AdvertisingCampaignUnauthorized struct {
}

// NewAdvertisingCampaignUnauthorized creates AdvertisingCampaignUnauthorized with default headers values
func NewAdvertisingCampaignUnauthorized() *AdvertisingCampaignUnauthorized {

	return &AdvertisingCampaignUnauthorized{}
}

// WriteResponse to the client
func (o *AdvertisingCampaignUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *AdvertisingCampaignUnauthorized) AdvertisingCampaignResponder() {}

// AdvertisingCampaignForbiddenCode is the HTTP code returned for type AdvertisingCampaignForbidden
const AdvertisingCampaignForbiddenCode int = 403

/*
AdvertisingCampaignForbidden Access forbiddenn

swagger:response advertisingCampaignForbidden
*/
type AdvertisingCampaignForbidden struct {
}

// NewAdvertisingCampaignForbidden creates AdvertisingCampaignForbidden with default headers values
func NewAdvertisingCampaignForbidden() *AdvertisingCampaignForbidden {

	return &AdvertisingCampaignForbidden{}
}

// WriteResponse to the client
func (o *AdvertisingCampaignForbidden) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(403)
}

func (o *AdvertisingCampaignForbidden) AdvertisingCampaignResponder() {}

// AdvertisingCampaignInternalServerErrorCode is the HTTP code returned for type AdvertisingCampaignInternalServerError
const AdvertisingCampaignInternalServerErrorCode int = 500

/*
AdvertisingCampaignInternalServerError internal error

swagger:response advertisingCampaignInternalServerError
*/
type AdvertisingCampaignInternalServerError struct {
}

// NewAdvertisingCampaignInternalServerError creates AdvertisingCampaignInternalServerError with default headers values
func NewAdvertisingCampaignInternalServerError() *AdvertisingCampaignInternalServerError {

	return &AdvertisingCampaignInternalServerError{}
}

// WriteResponse to the client
func (o *AdvertisingCampaignInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *AdvertisingCampaignInternalServerError) AdvertisingCampaignResponder() {}

type AdvertisingCampaignNotImplementedResponder struct {
	middleware.Responder
}

func (*AdvertisingCampaignNotImplementedResponder) AdvertisingCampaignResponder() {}

func AdvertisingCampaignNotImplemented() AdvertisingCampaignResponder {
	return &AdvertisingCampaignNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.AdvertisingCampaign has not yet been implemented",
		),
	}
}

type AdvertisingCampaignResponder interface {
	middleware.Responder
	AdvertisingCampaignResponder()
}
