// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetConfigVarIntNoContentCode is the HTTP code returned for type SetConfigVarIntNoContent
const SetConfigVarIntNoContentCode int = 204

/*
SetConfigVarIntNoContent OK

swagger:response setConfigVarIntNoContent
*/
type SetConfigVarIntNoContent struct {
}

// NewSetConfigVarIntNoContent creates SetConfigVarIntNoContent with default headers values
func NewSetConfigVarIntNoContent() *SetConfigVarIntNoContent {

	return &SetConfigVarIntNoContent{}
}

// WriteResponse to the client
func (o *SetConfigVarIntNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetConfigVarIntNoContent) SetConfigVarIntResponder() {}

// SetConfigVarIntInternalServerErrorCode is the HTTP code returned for type SetConfigVarIntInternalServerError
const SetConfigVarIntInternalServerErrorCode int = 500

/*
SetConfigVarIntInternalServerError Internal error

swagger:response setConfigVarIntInternalServerError
*/
type SetConfigVarIntInternalServerError struct {
}

// NewSetConfigVarIntInternalServerError creates SetConfigVarIntInternalServerError with default headers values
func NewSetConfigVarIntInternalServerError() *SetConfigVarIntInternalServerError {

	return &SetConfigVarIntInternalServerError{}
}

// WriteResponse to the client
func (o *SetConfigVarIntInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetConfigVarIntInternalServerError) SetConfigVarIntResponder() {}

type SetConfigVarIntNotImplementedResponder struct {
	middleware.Responder
}

func (*SetConfigVarIntNotImplementedResponder) SetConfigVarIntResponder() {}

func SetConfigVarIntNotImplemented() SetConfigVarIntResponder {
	return &SetConfigVarIntNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetConfigVarInt has not yet been implemented",
		),
	}
}

type SetConfigVarIntResponder interface {
	middleware.Responder
	SetConfigVarIntResponder()
}
