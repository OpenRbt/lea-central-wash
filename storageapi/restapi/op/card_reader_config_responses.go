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

// CardReaderConfigOKCode is the HTTP code returned for type CardReaderConfigOK
const CardReaderConfigOKCode int = 200

/*
CardReaderConfigOK OK

swagger:response cardReaderConfigOK
*/
type CardReaderConfigOK struct {

	/*
	  In: Body
	*/
	Payload *model.CardReaderConfig `json:"body,omitempty"`
}

// NewCardReaderConfigOK creates CardReaderConfigOK with default headers values
func NewCardReaderConfigOK() *CardReaderConfigOK {

	return &CardReaderConfigOK{}
}

// WithPayload adds the payload to the card reader config o k response
func (o *CardReaderConfigOK) WithPayload(payload *model.CardReaderConfig) *CardReaderConfigOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the card reader config o k response
func (o *CardReaderConfigOK) SetPayload(payload *model.CardReaderConfig) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *CardReaderConfigOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *CardReaderConfigOK) CardReaderConfigResponder() {}

// CardReaderConfigNotFoundCode is the HTTP code returned for type CardReaderConfigNotFound
const CardReaderConfigNotFoundCode int = 404

/*
CardReaderConfigNotFound not found

swagger:response cardReaderConfigNotFound
*/
type CardReaderConfigNotFound struct {
}

// NewCardReaderConfigNotFound creates CardReaderConfigNotFound with default headers values
func NewCardReaderConfigNotFound() *CardReaderConfigNotFound {

	return &CardReaderConfigNotFound{}
}

// WriteResponse to the client
func (o *CardReaderConfigNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *CardReaderConfigNotFound) CardReaderConfigResponder() {}

// CardReaderConfigInternalServerErrorCode is the HTTP code returned for type CardReaderConfigInternalServerError
const CardReaderConfigInternalServerErrorCode int = 500

/*
CardReaderConfigInternalServerError internal error

swagger:response cardReaderConfigInternalServerError
*/
type CardReaderConfigInternalServerError struct {
}

// NewCardReaderConfigInternalServerError creates CardReaderConfigInternalServerError with default headers values
func NewCardReaderConfigInternalServerError() *CardReaderConfigInternalServerError {

	return &CardReaderConfigInternalServerError{}
}

// WriteResponse to the client
func (o *CardReaderConfigInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *CardReaderConfigInternalServerError) CardReaderConfigResponder() {}

type CardReaderConfigNotImplementedResponder struct {
	middleware.Responder
}

func (*CardReaderConfigNotImplementedResponder) CardReaderConfigResponder() {}

func CardReaderConfigNotImplemented() CardReaderConfigResponder {
	return &CardReaderConfigNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.CardReaderConfig has not yet been implemented",
		),
	}
}

type CardReaderConfigResponder interface {
	middleware.Responder
	CardReaderConfigResponder()
}
