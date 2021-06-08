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

// StationReportCurrentMoneyOKCode is the HTTP code returned for type StationReportCurrentMoneyOK
const StationReportCurrentMoneyOKCode int = 200

/*StationReportCurrentMoneyOK OK

swagger:response stationReportCurrentMoneyOK
*/
type StationReportCurrentMoneyOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationReport `json:"body,omitempty"`
}

// NewStationReportCurrentMoneyOK creates StationReportCurrentMoneyOK with default headers values
func NewStationReportCurrentMoneyOK() *StationReportCurrentMoneyOK {

	return &StationReportCurrentMoneyOK{}
}

// WithPayload adds the payload to the station report current money o k response
func (o *StationReportCurrentMoneyOK) WithPayload(payload *model.StationReport) *StationReportCurrentMoneyOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station report current money o k response
func (o *StationReportCurrentMoneyOK) SetPayload(payload *model.StationReport) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationReportCurrentMoneyOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationReportCurrentMoneyOK) StationReportCurrentMoneyResponder() {}

// StationReportCurrentMoneyNotFoundCode is the HTTP code returned for type StationReportCurrentMoneyNotFound
const StationReportCurrentMoneyNotFoundCode int = 404

/*StationReportCurrentMoneyNotFound not found

swagger:response stationReportCurrentMoneyNotFound
*/
type StationReportCurrentMoneyNotFound struct {
}

// NewStationReportCurrentMoneyNotFound creates StationReportCurrentMoneyNotFound with default headers values
func NewStationReportCurrentMoneyNotFound() *StationReportCurrentMoneyNotFound {

	return &StationReportCurrentMoneyNotFound{}
}

// WriteResponse to the client
func (o *StationReportCurrentMoneyNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *StationReportCurrentMoneyNotFound) StationReportCurrentMoneyResponder() {}

// StationReportCurrentMoneyInternalServerErrorCode is the HTTP code returned for type StationReportCurrentMoneyInternalServerError
const StationReportCurrentMoneyInternalServerErrorCode int = 500

/*StationReportCurrentMoneyInternalServerError internal error

swagger:response stationReportCurrentMoneyInternalServerError
*/
type StationReportCurrentMoneyInternalServerError struct {
}

// NewStationReportCurrentMoneyInternalServerError creates StationReportCurrentMoneyInternalServerError with default headers values
func NewStationReportCurrentMoneyInternalServerError() *StationReportCurrentMoneyInternalServerError {

	return &StationReportCurrentMoneyInternalServerError{}
}

// WriteResponse to the client
func (o *StationReportCurrentMoneyInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationReportCurrentMoneyInternalServerError) StationReportCurrentMoneyResponder() {}

type StationReportCurrentMoneyNotImplementedResponder struct {
	middleware.Responder
}

func (*StationReportCurrentMoneyNotImplementedResponder) StationReportCurrentMoneyResponder() {}

func StationReportCurrentMoneyNotImplemented() StationReportCurrentMoneyResponder {
	return &StationReportCurrentMoneyNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationReportCurrentMoney has not yet been implemented",
		),
	}
}

type StationReportCurrentMoneyResponder interface {
	middleware.Responder
	StationReportCurrentMoneyResponder()
}
