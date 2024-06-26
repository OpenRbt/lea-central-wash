// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetConfigVarStringNoContentCode is the HTTP code returned for type SetConfigVarStringNoContent
const SetConfigVarStringNoContentCode int = 204

/*
SetConfigVarStringNoContent OK

swagger:response setConfigVarStringNoContent
*/
type SetConfigVarStringNoContent struct {
}

// NewSetConfigVarStringNoContent creates SetConfigVarStringNoContent with default headers values
func NewSetConfigVarStringNoContent() *SetConfigVarStringNoContent {

	return &SetConfigVarStringNoContent{}
}

// WriteResponse to the client
func (o *SetConfigVarStringNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetConfigVarStringNoContent) SetConfigVarStringResponder() {}

// SetConfigVarStringInternalServerErrorCode is the HTTP code returned for type SetConfigVarStringInternalServerError
const SetConfigVarStringInternalServerErrorCode int = 500

/*
SetConfigVarStringInternalServerError Internal error

swagger:response setConfigVarStringInternalServerError
*/
type SetConfigVarStringInternalServerError struct {
}

// NewSetConfigVarStringInternalServerError creates SetConfigVarStringInternalServerError with default headers values
func NewSetConfigVarStringInternalServerError() *SetConfigVarStringInternalServerError {

	return &SetConfigVarStringInternalServerError{}
}

// WriteResponse to the client
func (o *SetConfigVarStringInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetConfigVarStringInternalServerError) SetConfigVarStringResponder() {}

type SetConfigVarStringNotImplementedResponder struct {
	middleware.Responder
}

func (*SetConfigVarStringNotImplementedResponder) SetConfigVarStringResponder() {}

func SetConfigVarStringNotImplemented() SetConfigVarStringResponder {
	return &SetConfigVarStringNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetConfigVarString has not yet been implemented",
		),
	}
}

type SetConfigVarStringResponder interface {
	middleware.Responder
	SetConfigVarStringResponder()
}
