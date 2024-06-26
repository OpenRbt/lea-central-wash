// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// StationReportDatesOKCode is the HTTP code returned for type StationReportDatesOK
const StationReportDatesOKCode int = 200

/*
StationReportDatesOK OK

swagger:response stationReportDatesOK
*/
type StationReportDatesOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationReport `json:"body,omitempty"`
}

// NewStationReportDatesOK creates StationReportDatesOK with default headers values
func NewStationReportDatesOK() *StationReportDatesOK {

	return &StationReportDatesOK{}
}

// WithPayload adds the payload to the station report dates o k response
func (o *StationReportDatesOK) WithPayload(payload *model.StationReport) *StationReportDatesOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station report dates o k response
func (o *StationReportDatesOK) SetPayload(payload *model.StationReport) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationReportDatesOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationReportDatesOK) StationReportDatesResponder() {}

// StationReportDatesNotFoundCode is the HTTP code returned for type StationReportDatesNotFound
const StationReportDatesNotFoundCode int = 404

/*
StationReportDatesNotFound not found

swagger:response stationReportDatesNotFound
*/
type StationReportDatesNotFound struct {
}

// NewStationReportDatesNotFound creates StationReportDatesNotFound with default headers values
func NewStationReportDatesNotFound() *StationReportDatesNotFound {

	return &StationReportDatesNotFound{}
}

// WriteResponse to the client
func (o *StationReportDatesNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *StationReportDatesNotFound) StationReportDatesResponder() {}

// StationReportDatesInternalServerErrorCode is the HTTP code returned for type StationReportDatesInternalServerError
const StationReportDatesInternalServerErrorCode int = 500

/*
StationReportDatesInternalServerError internal error

swagger:response stationReportDatesInternalServerError
*/
type StationReportDatesInternalServerError struct {
}

// NewStationReportDatesInternalServerError creates StationReportDatesInternalServerError with default headers values
func NewStationReportDatesInternalServerError() *StationReportDatesInternalServerError {

	return &StationReportDatesInternalServerError{}
}

// WriteResponse to the client
func (o *StationReportDatesInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationReportDatesInternalServerError) StationReportDatesResponder() {}

type StationReportDatesNotImplementedResponder struct {
	middleware.Responder
}

func (*StationReportDatesNotImplementedResponder) StationReportDatesResponder() {}

func StationReportDatesNotImplemented() StationReportDatesResponder {
	return &StationReportDatesNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationReportDates has not yet been implemented",
		),
	}
}

type StationReportDatesResponder interface {
	middleware.Responder
	StationReportDatesResponder()
}
