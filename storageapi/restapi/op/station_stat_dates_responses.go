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

// StationStatDatesOKCode is the HTTP code returned for type StationStatDatesOK
const StationStatDatesOKCode int = 200

/*StationStatDatesOK OK

swagger:response stationStatDatesOK
*/
type StationStatDatesOK struct {

	/*
	  In: Body
	*/
	Payload model.StationsStat `json:"body,omitempty"`
}

// NewStationStatDatesOK creates StationStatDatesOK with default headers values
func NewStationStatDatesOK() *StationStatDatesOK {

	return &StationStatDatesOK{}
}

// WithPayload adds the payload to the station stat dates o k response
func (o *StationStatDatesOK) WithPayload(payload model.StationsStat) *StationStatDatesOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station stat dates o k response
func (o *StationStatDatesOK) SetPayload(payload model.StationsStat) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationStatDatesOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

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

func (o *StationStatDatesOK) StationStatDatesResponder() {}

// StationStatDatesUnauthorizedCode is the HTTP code returned for type StationStatDatesUnauthorized
const StationStatDatesUnauthorizedCode int = 401

/*StationStatDatesUnauthorized PIN is missing or invalid

swagger:response stationStatDatesUnauthorized
*/
type StationStatDatesUnauthorized struct {
}

// NewStationStatDatesUnauthorized creates StationStatDatesUnauthorized with default headers values
func NewStationStatDatesUnauthorized() *StationStatDatesUnauthorized {

	return &StationStatDatesUnauthorized{}
}

// WriteResponse to the client
func (o *StationStatDatesUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *StationStatDatesUnauthorized) StationStatDatesResponder() {}

// StationStatDatesForbiddenCode is the HTTP code returned for type StationStatDatesForbidden
const StationStatDatesForbiddenCode int = 403

/*StationStatDatesForbidden Access forbiddenn

swagger:response stationStatDatesForbidden
*/
type StationStatDatesForbidden struct {
}

// NewStationStatDatesForbidden creates StationStatDatesForbidden with default headers values
func NewStationStatDatesForbidden() *StationStatDatesForbidden {

	return &StationStatDatesForbidden{}
}

// WriteResponse to the client
func (o *StationStatDatesForbidden) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(403)
}

func (o *StationStatDatesForbidden) StationStatDatesResponder() {}

// StationStatDatesInternalServerErrorCode is the HTTP code returned for type StationStatDatesInternalServerError
const StationStatDatesInternalServerErrorCode int = 500

/*StationStatDatesInternalServerError internal error

swagger:response stationStatDatesInternalServerError
*/
type StationStatDatesInternalServerError struct {
}

// NewStationStatDatesInternalServerError creates StationStatDatesInternalServerError with default headers values
func NewStationStatDatesInternalServerError() *StationStatDatesInternalServerError {

	return &StationStatDatesInternalServerError{}
}

// WriteResponse to the client
func (o *StationStatDatesInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationStatDatesInternalServerError) StationStatDatesResponder() {}

type StationStatDatesNotImplementedResponder struct {
	middleware.Responder
}

func (*StationStatDatesNotImplementedResponder) StationStatDatesResponder() {}

func StationStatDatesNotImplemented() StationStatDatesResponder {
	return &StationStatDatesNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationStatDates has not yet been implemented",
		),
	}
}

type StationStatDatesResponder interface {
	middleware.Responder
	StationStatDatesResponder()
}