// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// LoadFromStationOKCode is the HTTP code returned for type LoadFromStationOK
const LoadFromStationOKCode int = 200

/*LoadFromStationOK OK

swagger:response loadFromStationOK
*/
type LoadFromStationOK struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewLoadFromStationOK creates LoadFromStationOK with default headers values
func NewLoadFromStationOK() *LoadFromStationOK {

	return &LoadFromStationOK{}
}

// WithPayload adds the payload to the load from station o k response
func (o *LoadFromStationOK) WithPayload(payload string) *LoadFromStationOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the load from station o k response
func (o *LoadFromStationOK) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *LoadFromStationOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *LoadFromStationOK) LoadFromStationResponder() {}

// LoadFromStationNotFoundCode is the HTTP code returned for type LoadFromStationNotFound
const LoadFromStationNotFoundCode int = 404

/*LoadFromStationNotFound not found

swagger:response loadFromStationNotFound
*/
type LoadFromStationNotFound struct {
}

// NewLoadFromStationNotFound creates LoadFromStationNotFound with default headers values
func NewLoadFromStationNotFound() *LoadFromStationNotFound {

	return &LoadFromStationNotFound{}
}

// WriteResponse to the client
func (o *LoadFromStationNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *LoadFromStationNotFound) LoadFromStationResponder() {}

// LoadFromStationInternalServerErrorCode is the HTTP code returned for type LoadFromStationInternalServerError
const LoadFromStationInternalServerErrorCode int = 500

/*LoadFromStationInternalServerError internal error

swagger:response loadFromStationInternalServerError
*/
type LoadFromStationInternalServerError struct {
}

// NewLoadFromStationInternalServerError creates LoadFromStationInternalServerError with default headers values
func NewLoadFromStationInternalServerError() *LoadFromStationInternalServerError {

	return &LoadFromStationInternalServerError{}
}

// WriteResponse to the client
func (o *LoadFromStationInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *LoadFromStationInternalServerError) LoadFromStationResponder() {}

type LoadFromStationNotImplementedResponder struct {
	middleware.Responder
}

func (*LoadFromStationNotImplementedResponder) LoadFromStationResponder() {}

func LoadFromStationNotImplemented() LoadFromStationResponder {
	return &LoadFromStationNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.LoadFromStation has not yet been implemented",
		),
	}
}

type LoadFromStationResponder interface {
	middleware.Responder
	LoadFromStationResponder()
}
