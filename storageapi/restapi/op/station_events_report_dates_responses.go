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

// StationEventsReportDatesOKCode is the HTTP code returned for type StationEventsReportDatesOK
const StationEventsReportDatesOKCode int = 200

/*StationEventsReportDatesOK ok

swagger:response stationEventsReportDatesOK
*/
type StationEventsReportDatesOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationEventReport `json:"body,omitempty"`
}

// NewStationEventsReportDatesOK creates StationEventsReportDatesOK with default headers values
func NewStationEventsReportDatesOK() *StationEventsReportDatesOK {

	return &StationEventsReportDatesOK{}
}

// WithPayload adds the payload to the station events report dates o k response
func (o *StationEventsReportDatesOK) WithPayload(payload *model.StationEventReport) *StationEventsReportDatesOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station events report dates o k response
func (o *StationEventsReportDatesOK) SetPayload(payload *model.StationEventReport) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationEventsReportDatesOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationEventsReportDatesOK) StationEventsReportDatesResponder() {}

// StationEventsReportDatesInternalServerErrorCode is the HTTP code returned for type StationEventsReportDatesInternalServerError
const StationEventsReportDatesInternalServerErrorCode int = 500

/*StationEventsReportDatesInternalServerError internal error

swagger:response stationEventsReportDatesInternalServerError
*/
type StationEventsReportDatesInternalServerError struct {
}

// NewStationEventsReportDatesInternalServerError creates StationEventsReportDatesInternalServerError with default headers values
func NewStationEventsReportDatesInternalServerError() *StationEventsReportDatesInternalServerError {

	return &StationEventsReportDatesInternalServerError{}
}

// WriteResponse to the client
func (o *StationEventsReportDatesInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationEventsReportDatesInternalServerError) StationEventsReportDatesResponder() {}

type StationEventsReportDatesNotImplementedResponder struct {
	middleware.Responder
}

func (*StationEventsReportDatesNotImplementedResponder) StationEventsReportDatesResponder() {}

func StationEventsReportDatesNotImplemented() StationEventsReportDatesResponder {
	return &StationEventsReportDatesNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationEventsReportDates has not yet been implemented",
		),
	}
}

type StationEventsReportDatesResponder interface {
	middleware.Responder
	StationEventsReportDatesResponder()
}
