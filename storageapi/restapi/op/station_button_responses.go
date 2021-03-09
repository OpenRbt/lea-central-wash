// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// StationButtonOKCode is the HTTP code returned for type StationButtonOK
const StationButtonOKCode int = 200

/*StationButtonOK OK

swagger:response stationButtonOK
*/
type StationButtonOK struct {

	/*
	  In: Body
	*/
	Payload *StationButtonOKBody `json:"body,omitempty"`
}

// NewStationButtonOK creates StationButtonOK with default headers values
func NewStationButtonOK() *StationButtonOK {

	return &StationButtonOK{}
}

// WithPayload adds the payload to the station button o k response
func (o *StationButtonOK) WithPayload(payload *StationButtonOKBody) *StationButtonOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station button o k response
func (o *StationButtonOK) SetPayload(payload *StationButtonOKBody) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationButtonOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationButtonOK) StationButtonResponder() {}

// StationButtonInternalServerErrorCode is the HTTP code returned for type StationButtonInternalServerError
const StationButtonInternalServerErrorCode int = 500

/*StationButtonInternalServerError internal error

swagger:response stationButtonInternalServerError
*/
type StationButtonInternalServerError struct {
}

// NewStationButtonInternalServerError creates StationButtonInternalServerError with default headers values
func NewStationButtonInternalServerError() *StationButtonInternalServerError {

	return &StationButtonInternalServerError{}
}

// WriteResponse to the client
func (o *StationButtonInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationButtonInternalServerError) StationButtonResponder() {}

type StationButtonNotImplementedResponder struct {
	middleware.Responder
}

func (*StationButtonNotImplementedResponder) StationButtonResponder() {}

func StationButtonNotImplemented() StationButtonResponder {
	return &StationButtonNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationButton has not yet been implemented",
		),
	}
}

type StationButtonResponder interface {
	middleware.Responder
	StationButtonResponder()
}