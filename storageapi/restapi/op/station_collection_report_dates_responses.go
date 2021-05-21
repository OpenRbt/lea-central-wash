// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// StationCollectionReportDatesOKCode is the HTTP code returned for type StationCollectionReportDatesOK
const StationCollectionReportDatesOKCode int = 200

/*StationCollectionReportDatesOK OK

swagger:response stationCollectionReportDatesOK
*/
type StationCollectionReportDatesOK struct {

	/*
	  In: Body
	*/
	Payload *StationCollectionReportDatesOKBody `json:"body,omitempty"`
}

// NewStationCollectionReportDatesOK creates StationCollectionReportDatesOK with default headers values
func NewStationCollectionReportDatesOK() *StationCollectionReportDatesOK {

	return &StationCollectionReportDatesOK{}
}

// WithPayload adds the payload to the station collection report dates o k response
func (o *StationCollectionReportDatesOK) WithPayload(payload *StationCollectionReportDatesOKBody) *StationCollectionReportDatesOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station collection report dates o k response
func (o *StationCollectionReportDatesOK) SetPayload(payload *StationCollectionReportDatesOKBody) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationCollectionReportDatesOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationCollectionReportDatesOK) StationCollectionReportDatesResponder() {}

// StationCollectionReportDatesNotFoundCode is the HTTP code returned for type StationCollectionReportDatesNotFound
const StationCollectionReportDatesNotFoundCode int = 404

/*StationCollectionReportDatesNotFound not found

swagger:response stationCollectionReportDatesNotFound
*/
type StationCollectionReportDatesNotFound struct {
}

// NewStationCollectionReportDatesNotFound creates StationCollectionReportDatesNotFound with default headers values
func NewStationCollectionReportDatesNotFound() *StationCollectionReportDatesNotFound {

	return &StationCollectionReportDatesNotFound{}
}

// WriteResponse to the client
func (o *StationCollectionReportDatesNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *StationCollectionReportDatesNotFound) StationCollectionReportDatesResponder() {}

// StationCollectionReportDatesInternalServerErrorCode is the HTTP code returned for type StationCollectionReportDatesInternalServerError
const StationCollectionReportDatesInternalServerErrorCode int = 500

/*StationCollectionReportDatesInternalServerError internal error

swagger:response stationCollectionReportDatesInternalServerError
*/
type StationCollectionReportDatesInternalServerError struct {
}

// NewStationCollectionReportDatesInternalServerError creates StationCollectionReportDatesInternalServerError with default headers values
func NewStationCollectionReportDatesInternalServerError() *StationCollectionReportDatesInternalServerError {

	return &StationCollectionReportDatesInternalServerError{}
}

// WriteResponse to the client
func (o *StationCollectionReportDatesInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationCollectionReportDatesInternalServerError) StationCollectionReportDatesResponder() {}

type StationCollectionReportDatesNotImplementedResponder struct {
	middleware.Responder
}

func (*StationCollectionReportDatesNotImplementedResponder) StationCollectionReportDatesResponder() {}

func StationCollectionReportDatesNotImplemented() StationCollectionReportDatesResponder {
	return &StationCollectionReportDatesNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationCollectionReportDates has not yet been implemented",
		),
	}
}

type StationCollectionReportDatesResponder interface {
	middleware.Responder
	StationCollectionReportDatesResponder()
}
