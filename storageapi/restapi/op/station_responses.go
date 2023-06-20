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

// StationOKCode is the HTTP code returned for type StationOK
const StationOKCode int = 200

/*StationOK OK

swagger:response stationOK
*/
type StationOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationConfig `json:"body,omitempty"`
}

// NewStationOK creates StationOK with default headers values
func NewStationOK() *StationOK {

	return &StationOK{}
}

// WithPayload adds the payload to the station o k response
func (o *StationOK) WithPayload(payload *model.StationConfig) *StationOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station o k response
func (o *StationOK) SetPayload(payload *model.StationConfig) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationOK) StationResponder() {}

// StationUnauthorizedCode is the HTTP code returned for type StationUnauthorized
const StationUnauthorizedCode int = 401

/*StationUnauthorized Access denied. It will happen when you try to change the ID at the station online.

swagger:response stationUnauthorized
*/
type StationUnauthorized struct {
}

// NewStationUnauthorized creates StationUnauthorized with default headers values
func NewStationUnauthorized() *StationUnauthorized {

	return &StationUnauthorized{}
}

// WriteResponse to the client
func (o *StationUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *StationUnauthorized) StationResponder() {}

// StationNotFoundCode is the HTTP code returned for type StationNotFound
const StationNotFoundCode int = 404

/*StationNotFound not found

swagger:response stationNotFound
*/
type StationNotFound struct {
}

// NewStationNotFound creates StationNotFound with default headers values
func NewStationNotFound() *StationNotFound {

	return &StationNotFound{}
}

// WriteResponse to the client
func (o *StationNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *StationNotFound) StationResponder() {}

// StationInternalServerErrorCode is the HTTP code returned for type StationInternalServerError
const StationInternalServerErrorCode int = 500

/*StationInternalServerError internal error

swagger:response stationInternalServerError
*/
type StationInternalServerError struct {
}

// NewStationInternalServerError creates StationInternalServerError with default headers values
func NewStationInternalServerError() *StationInternalServerError {

	return &StationInternalServerError{}
}

// WriteResponse to the client
func (o *StationInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationInternalServerError) StationResponder() {}

type StationNotImplementedResponder struct {
	middleware.Responder
}

func (*StationNotImplementedResponder) StationResponder() {}

func StationNotImplemented() StationResponder {
	return &StationNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.Station has not yet been implemented",
		),
	}
}

type StationResponder interface {
	middleware.Responder
	StationResponder()
}
