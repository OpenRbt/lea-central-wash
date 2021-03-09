// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// StationProgramByHashOKCode is the HTTP code returned for type StationProgramByHashOK
const StationProgramByHashOKCode int = 200

/*StationProgramByHashOK OK

swagger:response stationProgramByHashOK
*/
type StationProgramByHashOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationPrograms `json:"body,omitempty"`
}

// NewStationProgramByHashOK creates StationProgramByHashOK with default headers values
func NewStationProgramByHashOK() *StationProgramByHashOK {

	return &StationProgramByHashOK{}
}

// WithPayload adds the payload to the station program by hash o k response
func (o *StationProgramByHashOK) WithPayload(payload *model.StationPrograms) *StationProgramByHashOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the station program by hash o k response
func (o *StationProgramByHashOK) SetPayload(payload *model.StationPrograms) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *StationProgramByHashOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *StationProgramByHashOK) StationProgramByHashResponder() {}

// StationProgramByHashInternalServerErrorCode is the HTTP code returned for type StationProgramByHashInternalServerError
const StationProgramByHashInternalServerErrorCode int = 500

/*StationProgramByHashInternalServerError internal error

swagger:response stationProgramByHashInternalServerError
*/
type StationProgramByHashInternalServerError struct {
}

// NewStationProgramByHashInternalServerError creates StationProgramByHashInternalServerError with default headers values
func NewStationProgramByHashInternalServerError() *StationProgramByHashInternalServerError {

	return &StationProgramByHashInternalServerError{}
}

// WriteResponse to the client
func (o *StationProgramByHashInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *StationProgramByHashInternalServerError) StationProgramByHashResponder() {}

type StationProgramByHashNotImplementedResponder struct {
	middleware.Responder
}

func (*StationProgramByHashNotImplementedResponder) StationProgramByHashResponder() {}

func StationProgramByHashNotImplemented() StationProgramByHashResponder {
	return &StationProgramByHashNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.StationProgramByHash has not yet been implemented",
		),
	}
}

type StationProgramByHashResponder interface {
	middleware.Responder
	StationProgramByHashResponder()
}