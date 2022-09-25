// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetConfigVarBoolNoContentCode is the HTTP code returned for type SetConfigVarBoolNoContent
const SetConfigVarBoolNoContentCode int = 204

/*
SetConfigVarBoolNoContent OK

swagger:response setConfigVarBoolNoContent
*/
type SetConfigVarBoolNoContent struct {
}

// NewSetConfigVarBoolNoContent creates SetConfigVarBoolNoContent with default headers values
func NewSetConfigVarBoolNoContent() *SetConfigVarBoolNoContent {

	return &SetConfigVarBoolNoContent{}
}

// WriteResponse to the client
func (o *SetConfigVarBoolNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetConfigVarBoolNoContent) SetConfigVarBoolResponder() {}

// SetConfigVarBoolInternalServerErrorCode is the HTTP code returned for type SetConfigVarBoolInternalServerError
const SetConfigVarBoolInternalServerErrorCode int = 500

/*
SetConfigVarBoolInternalServerError Internal error

swagger:response setConfigVarBoolInternalServerError
*/
type SetConfigVarBoolInternalServerError struct {
}

// NewSetConfigVarBoolInternalServerError creates SetConfigVarBoolInternalServerError with default headers values
func NewSetConfigVarBoolInternalServerError() *SetConfigVarBoolInternalServerError {

	return &SetConfigVarBoolInternalServerError{}
}

// WriteResponse to the client
func (o *SetConfigVarBoolInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetConfigVarBoolInternalServerError) SetConfigVarBoolResponder() {}

type SetConfigVarBoolNotImplementedResponder struct {
	middleware.Responder
}

func (*SetConfigVarBoolNotImplementedResponder) SetConfigVarBoolResponder() {}

func SetConfigVarBoolNotImplemented() SetConfigVarBoolResponder {
	return &SetConfigVarBoolNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetConfigVarBool has not yet been implemented",
		),
	}
}

type SetConfigVarBoolResponder interface {
	middleware.Responder
	SetConfigVarBoolResponder()
}
