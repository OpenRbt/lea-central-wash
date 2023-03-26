// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// EndhSessionNoContentCode is the HTTP code returned for type EndhSessionNoContent
const EndhSessionNoContentCode int = 204

/*
EndhSessionNoContent OK

swagger:response endhSessionNoContent
*/
type EndhSessionNoContent struct {
}

// NewEndhSessionNoContent creates EndhSessionNoContent with default headers values
func NewEndhSessionNoContent() *EndhSessionNoContent {

	return &EndhSessionNoContent{}
}

// WriteResponse to the client
func (o *EndhSessionNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *EndhSessionNoContent) EndhSessionResponder() {}

// EndhSessionNotFoundCode is the HTTP code returned for type EndhSessionNotFound
const EndhSessionNotFoundCode int = 404

/*
EndhSessionNotFound hash not found

swagger:response endhSessionNotFound
*/
type EndhSessionNotFound struct {
}

// NewEndhSessionNotFound creates EndhSessionNotFound with default headers values
func NewEndhSessionNotFound() *EndhSessionNotFound {

	return &EndhSessionNotFound{}
}

// WriteResponse to the client
func (o *EndhSessionNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *EndhSessionNotFound) EndhSessionResponder() {}

// EndhSessionInternalServerErrorCode is the HTTP code returned for type EndhSessionInternalServerError
const EndhSessionInternalServerErrorCode int = 500

/*
EndhSessionInternalServerError Internal error

swagger:response endhSessionInternalServerError
*/
type EndhSessionInternalServerError struct {
}

// NewEndhSessionInternalServerError creates EndhSessionInternalServerError with default headers values
func NewEndhSessionInternalServerError() *EndhSessionInternalServerError {

	return &EndhSessionInternalServerError{}
}

// WriteResponse to the client
func (o *EndhSessionInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *EndhSessionInternalServerError) EndhSessionResponder() {}

type EndhSessionNotImplementedResponder struct {
	middleware.Responder
}

func (*EndhSessionNotImplementedResponder) EndhSessionResponder() {}

func EndhSessionNotImplemented() EndhSessionResponder {
	return &EndhSessionNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.EndhSession has not yet been implemented",
		),
	}
}

type EndhSessionResponder interface {
	middleware.Responder
	EndhSessionResponder()
}