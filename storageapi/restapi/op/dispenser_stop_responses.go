// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// DispenserStopNoContentCode is the HTTP code returned for type DispenserStopNoContent
const DispenserStopNoContentCode int = 204

/*
DispenserStopNoContent OK

swagger:response dispenserStopNoContent
*/
type DispenserStopNoContent struct {
}

// NewDispenserStopNoContent creates DispenserStopNoContent with default headers values
func NewDispenserStopNoContent() *DispenserStopNoContent {

	return &DispenserStopNoContent{}
}

// WriteResponse to the client
func (o *DispenserStopNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *DispenserStopNoContent) DispenserStopResponder() {}

// DispenserStopNotFoundCode is the HTTP code returned for type DispenserStopNotFound
const DispenserStopNotFoundCode int = 404

/*
DispenserStopNotFound not found

swagger:response dispenserStopNotFound
*/
type DispenserStopNotFound struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewDispenserStopNotFound creates DispenserStopNotFound with default headers values
func NewDispenserStopNotFound() *DispenserStopNotFound {

	return &DispenserStopNotFound{}
}

// WithPayload adds the payload to the dispenser stop not found response
func (o *DispenserStopNotFound) WithPayload(payload string) *DispenserStopNotFound {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the dispenser stop not found response
func (o *DispenserStopNotFound) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *DispenserStopNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(404)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *DispenserStopNotFound) DispenserStopResponder() {}

// DispenserStopInternalServerErrorCode is the HTTP code returned for type DispenserStopInternalServerError
const DispenserStopInternalServerErrorCode int = 500

/*
DispenserStopInternalServerError internal error

swagger:response dispenserStopInternalServerError
*/
type DispenserStopInternalServerError struct {
}

// NewDispenserStopInternalServerError creates DispenserStopInternalServerError with default headers values
func NewDispenserStopInternalServerError() *DispenserStopInternalServerError {

	return &DispenserStopInternalServerError{}
}

// WriteResponse to the client
func (o *DispenserStopInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *DispenserStopInternalServerError) DispenserStopResponder() {}

type DispenserStopNotImplementedResponder struct {
	middleware.Responder
}

func (*DispenserStopNotImplementedResponder) DispenserStopResponder() {}

func DispenserStopNotImplemented() DispenserStopResponder {
	return &DispenserStopNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.DispenserStop has not yet been implemented",
		),
	}
}

type DispenserStopResponder interface {
	middleware.Responder
	DispenserStopResponder()
}
