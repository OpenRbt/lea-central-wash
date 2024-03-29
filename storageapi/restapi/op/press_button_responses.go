// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// PressButtonNoContentCode is the HTTP code returned for type PressButtonNoContent
const PressButtonNoContentCode int = 204

/*
PressButtonNoContent OK

swagger:response pressButtonNoContent
*/
type PressButtonNoContent struct {
}

// NewPressButtonNoContent creates PressButtonNoContent with default headers values
func NewPressButtonNoContent() *PressButtonNoContent {

	return &PressButtonNoContent{}
}

// WriteResponse to the client
func (o *PressButtonNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *PressButtonNoContent) PressButtonResponder() {}

// PressButtonNotFoundCode is the HTTP code returned for type PressButtonNotFound
const PressButtonNotFoundCode int = 404

/*
PressButtonNotFound not found

swagger:response pressButtonNotFound
*/
type PressButtonNotFound struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewPressButtonNotFound creates PressButtonNotFound with default headers values
func NewPressButtonNotFound() *PressButtonNotFound {

	return &PressButtonNotFound{}
}

// WithPayload adds the payload to the press button not found response
func (o *PressButtonNotFound) WithPayload(payload string) *PressButtonNotFound {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the press button not found response
func (o *PressButtonNotFound) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *PressButtonNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(404)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *PressButtonNotFound) PressButtonResponder() {}

// PressButtonInternalServerErrorCode is the HTTP code returned for type PressButtonInternalServerError
const PressButtonInternalServerErrorCode int = 500

/*
PressButtonInternalServerError internal error

swagger:response pressButtonInternalServerError
*/
type PressButtonInternalServerError struct {
}

// NewPressButtonInternalServerError creates PressButtonInternalServerError with default headers values
func NewPressButtonInternalServerError() *PressButtonInternalServerError {

	return &PressButtonInternalServerError{}
}

// WriteResponse to the client
func (o *PressButtonInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *PressButtonInternalServerError) PressButtonResponder() {}

type PressButtonNotImplementedResponder struct {
	middleware.Responder
}

func (*PressButtonNotImplementedResponder) PressButtonResponder() {}

func PressButtonNotImplemented() PressButtonResponder {
	return &PressButtonNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.PressButton has not yet been implemented",
		),
	}
}

type PressButtonResponder interface {
	middleware.Responder
	PressButtonResponder()
}
