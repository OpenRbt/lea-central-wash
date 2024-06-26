// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetStationConfigVarIntNoContentCode is the HTTP code returned for type SetStationConfigVarIntNoContent
const SetStationConfigVarIntNoContentCode int = 204

/*
SetStationConfigVarIntNoContent OK

swagger:response setStationConfigVarIntNoContent
*/
type SetStationConfigVarIntNoContent struct {
}

// NewSetStationConfigVarIntNoContent creates SetStationConfigVarIntNoContent with default headers values
func NewSetStationConfigVarIntNoContent() *SetStationConfigVarIntNoContent {

	return &SetStationConfigVarIntNoContent{}
}

// WriteResponse to the client
func (o *SetStationConfigVarIntNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetStationConfigVarIntNoContent) SetStationConfigVarIntResponder() {}

// SetStationConfigVarIntNotFoundCode is the HTTP code returned for type SetStationConfigVarIntNotFound
const SetStationConfigVarIntNotFoundCode int = 404

/*
SetStationConfigVarIntNotFound Not found

swagger:response setStationConfigVarIntNotFound
*/
type SetStationConfigVarIntNotFound struct {
}

// NewSetStationConfigVarIntNotFound creates SetStationConfigVarIntNotFound with default headers values
func NewSetStationConfigVarIntNotFound() *SetStationConfigVarIntNotFound {

	return &SetStationConfigVarIntNotFound{}
}

// WriteResponse to the client
func (o *SetStationConfigVarIntNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *SetStationConfigVarIntNotFound) SetStationConfigVarIntResponder() {}

// SetStationConfigVarIntInternalServerErrorCode is the HTTP code returned for type SetStationConfigVarIntInternalServerError
const SetStationConfigVarIntInternalServerErrorCode int = 500

/*
SetStationConfigVarIntInternalServerError Internal error

swagger:response setStationConfigVarIntInternalServerError
*/
type SetStationConfigVarIntInternalServerError struct {
}

// NewSetStationConfigVarIntInternalServerError creates SetStationConfigVarIntInternalServerError with default headers values
func NewSetStationConfigVarIntInternalServerError() *SetStationConfigVarIntInternalServerError {

	return &SetStationConfigVarIntInternalServerError{}
}

// WriteResponse to the client
func (o *SetStationConfigVarIntInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetStationConfigVarIntInternalServerError) SetStationConfigVarIntResponder() {}

type SetStationConfigVarIntNotImplementedResponder struct {
	middleware.Responder
}

func (*SetStationConfigVarIntNotImplementedResponder) SetStationConfigVarIntResponder() {}

func SetStationConfigVarIntNotImplemented() SetStationConfigVarIntResponder {
	return &SetStationConfigVarIntNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetStationConfigVarInt has not yet been implemented",
		),
	}
}

type SetStationConfigVarIntResponder interface {
	middleware.Responder
	SetStationConfigVarIntResponder()
}
