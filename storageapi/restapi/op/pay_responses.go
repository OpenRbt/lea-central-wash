// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// PayNoContentCode is the HTTP code returned for type PayNoContent
const PayNoContentCode int = 204

/*
PayNoContent OK

swagger:response payNoContent
*/
type PayNoContent struct {
}

// NewPayNoContent creates PayNoContent with default headers values
func NewPayNoContent() *PayNoContent {

	return &PayNoContent{}
}

// WriteResponse to the client
func (o *PayNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *PayNoContent) PayResponder() {}

// PayUnauthorizedCode is the HTTP code returned for type PayUnauthorized
const PayUnauthorizedCode int = 401

/*
PayUnauthorized user not authorized

swagger:response payUnauthorized
*/
type PayUnauthorized struct {
}

// NewPayUnauthorized creates PayUnauthorized with default headers values
func NewPayUnauthorized() *PayUnauthorized {

	return &PayUnauthorized{}
}

// WriteResponse to the client
func (o *PayUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *PayUnauthorized) PayResponder() {}

// PayNotFoundCode is the HTTP code returned for type PayNotFound
const PayNotFoundCode int = 404

/*
PayNotFound hash not found

swagger:response payNotFound
*/
type PayNotFound struct {
}

// NewPayNotFound creates PayNotFound with default headers values
func NewPayNotFound() *PayNotFound {

	return &PayNotFound{}
}

// WriteResponse to the client
func (o *PayNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *PayNotFound) PayResponder() {}

// PayInternalServerErrorCode is the HTTP code returned for type PayInternalServerError
const PayInternalServerErrorCode int = 500

/*
PayInternalServerError Internal error

swagger:response payInternalServerError
*/
type PayInternalServerError struct {
}

// NewPayInternalServerError creates PayInternalServerError with default headers values
func NewPayInternalServerError() *PayInternalServerError {

	return &PayInternalServerError{}
}

// WriteResponse to the client
func (o *PayInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *PayInternalServerError) PayResponder() {}

type PayNotImplementedResponder struct {
	middleware.Responder
}

func (*PayNotImplementedResponder) PayResponder() {}

func PayNotImplemented() PayResponder {
	return &PayNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.Pay has not yet been implemented",
		),
	}
}

type PayResponder interface {
	middleware.Responder
	PayResponder()
}
