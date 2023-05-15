// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationStatCurrentOKCode is the HTTP code returned for type StationStatCurrentOK
const StationStatCurrentOKCode int = 200

/*
StationStatCurrentOK OK

swagger:response stationStatCurrentOK
*/
type StationStatCurrentOK struct {

	/*
	  In: Body
	*/
	Payload model.StationsStat `json:"body,omitempty"`
}

// NewStationStatCurrentOK creates StationStatCurrentOK with default headers values
func NewStationStatCurrentOK() *StationStatCurrentOK {

	return &StationStatCurrentOK{}
}

// WithPayload adds the payload to the station stat current o k response
func (o *StationStatCurrentOK) WithPayload(payload model.StationsStat) *StationStatCurrentOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station stat current o k response
func (o *StationStatCurrentOK) SetPayload(payload model.StationsStat) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationStatCurrentOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if payload == nil {
		// return empty array
		payload = model.StationsStat{}
	}

	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *StationStatCurrentOK) StationStatCurrentResponder() {}

// StationStatCurrentUnauthorizedCode is the HTTP code returned for type StationStatCurrentUnauthorized
const StationStatCurrentUnauthorizedCode int = 401

/*
StationStatCurrentUnauthorized PIN is missing or invalid

swagger:response stationStatCurrentUnauthorized
*/
type StationStatCurrentUnauthorized struct {
}

// NewStationStatCurrentUnauthorized creates StationStatCurrentUnauthorized with default headers values
func NewStationStatCurrentUnauthorized() *StationStatCurrentUnauthorized {

	return &StationStatCurrentUnauthorized{}
}

// WriteResponse to the client
func (o *StationStatCurrentUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *StationStatCurrentUnauthorized) StationStatCurrentResponder() {}

// StationStatCurrentForbiddenCode is the HTTP code returned for type StationStatCurrentForbidden
const StationStatCurrentForbiddenCode int = 403

/*
StationStatCurrentForbidden Access forbidden

swagger:response stationStatCurrentForbidden
*/
type StationStatCurrentForbidden struct {
}

// NewStationStatCurrentForbidden creates StationStatCurrentForbidden with default headers values
func NewStationStatCurrentForbidden() *StationStatCurrentForbidden {

	return &StationStatCurrentForbidden{}
}

// WriteResponse to the client
func (o *StationStatCurrentForbidden) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(403)
}

func (o *StationStatCurrentForbidden) StationStatCurrentResponder() {}

// StationStatCurrentInternalServerErrorCode is the HTTP code returned for type StationStatCurrentInternalServerError
const StationStatCurrentInternalServerErrorCode int = 500

/*
StationStatCurrentInternalServerError internal error

swagger:response stationStatCurrentInternalServerError
*/
type StationStatCurrentInternalServerError struct {
}

// NewStationStatCurrentInternalServerError creates StationStatCurrentInternalServerError with default headers values
func NewStationStatCurrentInternalServerError() *StationStatCurrentInternalServerError {

	return &StationStatCurrentInternalServerError{}
}

// WriteResponse to the client
func (o *StationStatCurrentInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationStatCurrentInternalServerError) StationStatCurrentResponder() {}

type StationStatCurrentNotImplementedResponder struct {
	middleware.Responder
}

func (*StationStatCurrentNotImplementedResponder) StationStatCurrentResponder() {}

func StationStatCurrentNotImplemented() StationStatCurrentResponder {
	return &StationStatCurrentNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationStatCurrent has not yet been implemented",
		),
	}
}

type StationStatCurrentResponder interface {
	middleware.Responder
	StationStatCurrentResponder()
}
