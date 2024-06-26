// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// Run2ProgramNoContentCode is the HTTP code returned for type Run2ProgramNoContent
const Run2ProgramNoContentCode int = 204

/*
Run2ProgramNoContent OK

swagger:response run2ProgramNoContent
*/
type Run2ProgramNoContent struct {
}

// NewRun2ProgramNoContent creates Run2ProgramNoContent with default headers values
func NewRun2ProgramNoContent() *Run2ProgramNoContent {

	return &Run2ProgramNoContent{}
}

// WriteResponse to the client
func (o *Run2ProgramNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *Run2ProgramNoContent) Run2ProgramResponder() {}

// Run2ProgramNotFoundCode is the HTTP code returned for type Run2ProgramNotFound
const Run2ProgramNotFoundCode int = 404

/*
Run2ProgramNotFound not found

swagger:response run2ProgramNotFound
*/
type Run2ProgramNotFound struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewRun2ProgramNotFound creates Run2ProgramNotFound with default headers values
func NewRun2ProgramNotFound() *Run2ProgramNotFound {

	return &Run2ProgramNotFound{}
}

// WithPayload adds the payload to the run2 program not found response
func (o *Run2ProgramNotFound) WithPayload(payload string) *Run2ProgramNotFound {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the run2 program not found response
func (o *Run2ProgramNotFound) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *Run2ProgramNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(404)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *Run2ProgramNotFound) Run2ProgramResponder() {}

// Run2ProgramInternalServerErrorCode is the HTTP code returned for type Run2ProgramInternalServerError
const Run2ProgramInternalServerErrorCode int = 500

/*
Run2ProgramInternalServerError internal error

swagger:response run2ProgramInternalServerError
*/
type Run2ProgramInternalServerError struct {
}

// NewRun2ProgramInternalServerError creates Run2ProgramInternalServerError with default headers values
func NewRun2ProgramInternalServerError() *Run2ProgramInternalServerError {

	return &Run2ProgramInternalServerError{}
}

// WriteResponse to the client
func (o *Run2ProgramInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *Run2ProgramInternalServerError) Run2ProgramResponder() {}

type Run2ProgramNotImplementedResponder struct {
	middleware.Responder
}

func (*Run2ProgramNotImplementedResponder) Run2ProgramResponder() {}

func Run2ProgramNotImplemented() Run2ProgramResponder {
	return &Run2ProgramNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.Run2Program has not yet been implemented",
		),
	}
}

type Run2ProgramResponder interface {
	middleware.Responder
	Run2ProgramResponder()
}
