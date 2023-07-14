// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// SetStationButtonNoContentCode is the HTTP code returned for type SetStationButtonNoContent
const SetStationButtonNoContentCode int = 204

/*
SetStationButtonNoContent OK

swagger:response setStationButtonNoContent
*/
type SetStationButtonNoContent struct {
}

// NewSetStationButtonNoContent creates SetStationButtonNoContent with default headers values
func NewSetStationButtonNoContent() *SetStationButtonNoContent {

	return &SetStationButtonNoContent{}
}

// WriteResponse to the client
func (o *SetStationButtonNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *SetStationButtonNoContent) SetStationButtonResponder() {}

// SetStationButtonUnprocessableEntityCode is the HTTP code returned for type SetStationButtonUnprocessableEntity
const SetStationButtonUnprocessableEntityCode int = 422

/*
SetStationButtonUnprocessableEntity validation error

swagger:response setStationButtonUnprocessableEntity
*/
type SetStationButtonUnprocessableEntity struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewSetStationButtonUnprocessableEntity creates SetStationButtonUnprocessableEntity with default headers values
func NewSetStationButtonUnprocessableEntity() *SetStationButtonUnprocessableEntity {

	return &SetStationButtonUnprocessableEntity{}
}

// WithPayload adds the payload to the set station button unprocessable entity response
func (o *SetStationButtonUnprocessableEntity) WithPayload(payload string) *SetStationButtonUnprocessableEntity {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the set station button unprocessable entity response
func (o *SetStationButtonUnprocessableEntity) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *SetStationButtonUnprocessableEntity) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(422)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *SetStationButtonUnprocessableEntity) SetStationButtonResponder() {}

// SetStationButtonInternalServerErrorCode is the HTTP code returned for type SetStationButtonInternalServerError
const SetStationButtonInternalServerErrorCode int = 500

/*
SetStationButtonInternalServerError internal error

swagger:response setStationButtonInternalServerError
*/
type SetStationButtonInternalServerError struct {
}

// NewSetStationButtonInternalServerError creates SetStationButtonInternalServerError with default headers values
func NewSetStationButtonInternalServerError() *SetStationButtonInternalServerError {

	return &SetStationButtonInternalServerError{}
}

// WriteResponse to the client
func (o *SetStationButtonInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *SetStationButtonInternalServerError) SetStationButtonResponder() {}

type SetStationButtonNotImplementedResponder struct {
	middleware.Responder
}

func (*SetStationButtonNotImplementedResponder) SetStationButtonResponder() {}

func SetStationButtonNotImplemented() SetStationButtonResponder {
	return &SetStationButtonNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.SetStationButton has not yet been implemented",
		),
	}
}

type SetStationButtonResponder interface {
	middleware.Responder
	SetStationButtonResponder()
}
