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

// GetStationConfigVarStringOKCode is the HTTP code returned for type GetStationConfigVarStringOK
const GetStationConfigVarStringOKCode int = 200

/*
GetStationConfigVarStringOK OK

swagger:response getStationConfigVarStringOK
*/
type GetStationConfigVarStringOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationConfigVarString `json:"body,omitempty"`
}

// NewGetStationConfigVarStringOK creates GetStationConfigVarStringOK with default headers values
func NewGetStationConfigVarStringOK() *GetStationConfigVarStringOK {

	return &GetStationConfigVarStringOK{}
}

// WithPayload adds the payload to the get station config var string o k response
func (o *GetStationConfigVarStringOK) WithPayload(payload *model.StationConfigVarString) *GetStationConfigVarStringOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get station config var string o k response
func (o *GetStationConfigVarStringOK) SetPayload(payload *model.StationConfigVarString) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetStationConfigVarStringOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetStationConfigVarStringOK) GetStationConfigVarStringResponder() {}

// GetStationConfigVarStringNotFoundCode is the HTTP code returned for type GetStationConfigVarStringNotFound
const GetStationConfigVarStringNotFoundCode int = 404

/*
GetStationConfigVarStringNotFound Not found

swagger:response getStationConfigVarStringNotFound
*/
type GetStationConfigVarStringNotFound struct {
}

// NewGetStationConfigVarStringNotFound creates GetStationConfigVarStringNotFound with default headers values
func NewGetStationConfigVarStringNotFound() *GetStationConfigVarStringNotFound {

	return &GetStationConfigVarStringNotFound{}
}

// WriteResponse to the client
func (o *GetStationConfigVarStringNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetStationConfigVarStringNotFound) GetStationConfigVarStringResponder() {}

// GetStationConfigVarStringInternalServerErrorCode is the HTTP code returned for type GetStationConfigVarStringInternalServerError
const GetStationConfigVarStringInternalServerErrorCode int = 500

/*
GetStationConfigVarStringInternalServerError Internal error

swagger:response getStationConfigVarStringInternalServerError
*/
type GetStationConfigVarStringInternalServerError struct {
}

// NewGetStationConfigVarStringInternalServerError creates GetStationConfigVarStringInternalServerError with default headers values
func NewGetStationConfigVarStringInternalServerError() *GetStationConfigVarStringInternalServerError {

	return &GetStationConfigVarStringInternalServerError{}
}

// WriteResponse to the client
func (o *GetStationConfigVarStringInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetStationConfigVarStringInternalServerError) GetStationConfigVarStringResponder() {}

type GetStationConfigVarStringNotImplementedResponder struct {
	middleware.Responder
}

func (*GetStationConfigVarStringNotImplementedResponder) GetStationConfigVarStringResponder() {}

func GetStationConfigVarStringNotImplemented() GetStationConfigVarStringResponder {
	return &GetStationConfigVarStringNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetStationConfigVarString has not yet been implemented",
		),
	}
}

type GetStationConfigVarStringResponder interface {
	middleware.Responder
	GetStationConfigVarStringResponder()
}