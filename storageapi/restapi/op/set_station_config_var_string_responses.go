// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetStationConfigVarStringNoContentCode is the HTTP code returned for type SetStationConfigVarStringNoContent
const SetStationConfigVarStringNoContentCode int = 204

/*
SetStationConfigVarStringNoContent OK

swagger:response setStationConfigVarStringNoContent
*/
type SetStationConfigVarStringNoContent struct {
}

// NewSetStationConfigVarStringNoContent creates SetStationConfigVarStringNoContent with default headers values
func NewSetStationConfigVarStringNoContent() *SetStationConfigVarStringNoContent {

	return &SetStationConfigVarStringNoContent{}
}

// WriteResponse to the client
func (o *SetStationConfigVarStringNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetStationConfigVarStringNoContent) SetStationConfigVarStringResponder() {}

// SetStationConfigVarStringInternalServerErrorCode is the HTTP code returned for type SetStationConfigVarStringInternalServerError
const SetStationConfigVarStringInternalServerErrorCode int = 500

/*
SetStationConfigVarStringInternalServerError Internal error

swagger:response setStationConfigVarStringInternalServerError
*/
type SetStationConfigVarStringInternalServerError struct {
}

// NewSetStationConfigVarStringInternalServerError creates SetStationConfigVarStringInternalServerError with default headers values
func NewSetStationConfigVarStringInternalServerError() *SetStationConfigVarStringInternalServerError {

	return &SetStationConfigVarStringInternalServerError{}
}

// WriteResponse to the client
func (o *SetStationConfigVarStringInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetStationConfigVarStringInternalServerError) SetStationConfigVarStringResponder() {}

type SetStationConfigVarStringNotImplementedResponder struct {
	middleware.Responder
}

func (*SetStationConfigVarStringNotImplementedResponder) SetStationConfigVarStringResponder() {}

func SetStationConfigVarStringNotImplemented() SetStationConfigVarStringResponder {
	return &SetStationConfigVarStringNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetStationConfigVarString has not yet been implemented",
		),
	}
}

type SetStationConfigVarStringResponder interface {
	middleware.Responder
	SetStationConfigVarStringResponder()
}
