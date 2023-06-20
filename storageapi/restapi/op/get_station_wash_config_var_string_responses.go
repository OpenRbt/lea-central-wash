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

// GetStationWashConfigVarStringOKCode is the HTTP code returned for type GetStationWashConfigVarStringOK
const GetStationWashConfigVarStringOKCode int = 200

/*GetStationWashConfigVarStringOK OK

swagger:response getStationWashConfigVarStringOK
*/
type GetStationWashConfigVarStringOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationConfigVarString `json:"body,omitempty"`
}

// NewGetStationWashConfigVarStringOK creates GetStationWashConfigVarStringOK with default headers values
func NewGetStationWashConfigVarStringOK() *GetStationWashConfigVarStringOK {

	return &GetStationWashConfigVarStringOK{}
}

// WithPayload adds the payload to the get station wash config var string o k response
func (o *GetStationWashConfigVarStringOK) WithPayload(payload *model.StationConfigVarString) *GetStationWashConfigVarStringOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get station wash config var string o k response
func (o *GetStationWashConfigVarStringOK) SetPayload(payload *model.StationConfigVarString) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetStationWashConfigVarStringOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetStationWashConfigVarStringOK) GetStationWashConfigVarStringResponder() {}

// GetStationWashConfigVarStringNotFoundCode is the HTTP code returned for type GetStationWashConfigVarStringNotFound
const GetStationWashConfigVarStringNotFoundCode int = 404

/*GetStationWashConfigVarStringNotFound Not found

swagger:response getStationWashConfigVarStringNotFound
*/
type GetStationWashConfigVarStringNotFound struct {
}

// NewGetStationWashConfigVarStringNotFound creates GetStationWashConfigVarStringNotFound with default headers values
func NewGetStationWashConfigVarStringNotFound() *GetStationWashConfigVarStringNotFound {

	return &GetStationWashConfigVarStringNotFound{}
}

// WriteResponse to the client
func (o *GetStationWashConfigVarStringNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetStationWashConfigVarStringNotFound) GetStationWashConfigVarStringResponder() {}

// GetStationWashConfigVarStringInternalServerErrorCode is the HTTP code returned for type GetStationWashConfigVarStringInternalServerError
const GetStationWashConfigVarStringInternalServerErrorCode int = 500

/*GetStationWashConfigVarStringInternalServerError Internal error

swagger:response getStationWashConfigVarStringInternalServerError
*/
type GetStationWashConfigVarStringInternalServerError struct {
}

// NewGetStationWashConfigVarStringInternalServerError creates GetStationWashConfigVarStringInternalServerError with default headers values
func NewGetStationWashConfigVarStringInternalServerError() *GetStationWashConfigVarStringInternalServerError {

	return &GetStationWashConfigVarStringInternalServerError{}
}

// WriteResponse to the client
func (o *GetStationWashConfigVarStringInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetStationWashConfigVarStringInternalServerError) GetStationWashConfigVarStringResponder() {}

type GetStationWashConfigVarStringNotImplementedResponder struct {
	middleware.Responder
}

func (*GetStationWashConfigVarStringNotImplementedResponder) GetStationWashConfigVarStringResponder() {
}

func GetStationWashConfigVarStringNotImplemented() GetStationWashConfigVarStringResponder {
	return &GetStationWashConfigVarStringNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetStationWashConfigVarString has not yet been implemented",
		),
	}
}

type GetStationWashConfigVarStringResponder interface {
	middleware.Responder
	GetStationWashConfigVarStringResponder()
}
