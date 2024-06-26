// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetKasseNoContentCode is the HTTP code returned for type SetKasseNoContent
const SetKasseNoContentCode int = 204

/*
SetKasseNoContent OK

swagger:response setKasseNoContent
*/
type SetKasseNoContent struct {
}

// NewSetKasseNoContent creates SetKasseNoContent with default headers values
func NewSetKasseNoContent() *SetKasseNoContent {

	return &SetKasseNoContent{}
}

// WriteResponse to the client
func (o *SetKasseNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetKasseNoContent) SetKasseResponder() {}

// SetKasseInternalServerErrorCode is the HTTP code returned for type SetKasseInternalServerError
const SetKasseInternalServerErrorCode int = 500

/*
SetKasseInternalServerError internal error

swagger:response setKasseInternalServerError
*/
type SetKasseInternalServerError struct {
}

// NewSetKasseInternalServerError creates SetKasseInternalServerError with default headers values
func NewSetKasseInternalServerError() *SetKasseInternalServerError {

	return &SetKasseInternalServerError{}
}

// WriteResponse to the client
func (o *SetKasseInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetKasseInternalServerError) SetKasseResponder() {}

type SetKasseNotImplementedResponder struct {
	middleware.Responder
}

func (*SetKasseNotImplementedResponder) SetKasseResponder() {}

func SetKasseNotImplemented() SetKasseResponder {
	return &SetKasseNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetKasse has not yet been implemented",
		),
	}
}

type SetKasseResponder interface {
	middleware.Responder
	SetKasseResponder()
}
