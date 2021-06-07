// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// CardReaderConfigByHashOKCode is the HTTP code returned for type CardReaderConfigByHashOK
const CardReaderConfigByHashOKCode int = 200

/*CardReaderConfigByHashOK OK

swagger:response cardReaderConfigByHashOK
*/
type CardReaderConfigByHashOK struct {

	/*
	  In: Body
	*/
	Payload *model.CardReaderConfig `json:"body,omitempty"`
}

// NewCardReaderConfigByHashOK creates CardReaderConfigByHashOK with default headers values
func NewCardReaderConfigByHashOK() *CardReaderConfigByHashOK {

	return &CardReaderConfigByHashOK{}
}

// WithPayload adds the payload to the card reader config by hash o k response
func (o *CardReaderConfigByHashOK) WithPayload(payload *model.CardReaderConfig) *CardReaderConfigByHashOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the card reader config by hash o k response
func (o *CardReaderConfigByHashOK) SetPayload(payload *model.CardReaderConfig) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *CardReaderConfigByHashOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *CardReaderConfigByHashOK) CardReaderConfigByHashResponder() {}

// CardReaderConfigByHashNotFoundCode is the HTTP code returned for type CardReaderConfigByHashNotFound
const CardReaderConfigByHashNotFoundCode int = 404

/*CardReaderConfigByHashNotFound not found

swagger:response cardReaderConfigByHashNotFound
*/
type CardReaderConfigByHashNotFound struct {
}

// NewCardReaderConfigByHashNotFound creates CardReaderConfigByHashNotFound with default headers values
func NewCardReaderConfigByHashNotFound() *CardReaderConfigByHashNotFound {

	return &CardReaderConfigByHashNotFound{}
}

// WriteResponse to the client
func (o *CardReaderConfigByHashNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *CardReaderConfigByHashNotFound) CardReaderConfigByHashResponder() {}

// CardReaderConfigByHashInternalServerErrorCode is the HTTP code returned for type CardReaderConfigByHashInternalServerError
const CardReaderConfigByHashInternalServerErrorCode int = 500

/*CardReaderConfigByHashInternalServerError internal error

swagger:response cardReaderConfigByHashInternalServerError
*/
type CardReaderConfigByHashInternalServerError struct {
}

// NewCardReaderConfigByHashInternalServerError creates CardReaderConfigByHashInternalServerError with default headers values
func NewCardReaderConfigByHashInternalServerError() *CardReaderConfigByHashInternalServerError {

	return &CardReaderConfigByHashInternalServerError{}
}

// WriteResponse to the client
func (o *CardReaderConfigByHashInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *CardReaderConfigByHashInternalServerError) CardReaderConfigByHashResponder() {}

type CardReaderConfigByHashNotImplementedResponder struct {
	middleware.Responder
}

func (*CardReaderConfigByHashNotImplementedResponder) CardReaderConfigByHashResponder() {}

func CardReaderConfigByHashNotImplemented() CardReaderConfigByHashResponder {
	return &CardReaderConfigByHashNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.CardReaderConfigByHash has not yet been implemented",
		),
	}
}

type CardReaderConfigByHashResponder interface {
	middleware.Responder
	CardReaderConfigByHashResponder()
}
