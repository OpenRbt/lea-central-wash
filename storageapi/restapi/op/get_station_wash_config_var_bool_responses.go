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

// GetStationWashConfigVarBoolOKCode is the HTTP code returned for type GetStationWashConfigVarBoolOK
const GetStationWashConfigVarBoolOKCode int = 200

/*
GetStationWashConfigVarBoolOK OK

swagger:response getStationWashConfigVarBoolOK
*/
type GetStationWashConfigVarBoolOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationConfigVarBool `json:"body,omitempty"`
}

// NewGetStationWashConfigVarBoolOK creates GetStationWashConfigVarBoolOK with default headers values
func NewGetStationWashConfigVarBoolOK() *GetStationWashConfigVarBoolOK {

	return &GetStationWashConfigVarBoolOK{}
}

// WithPayload adds the payload to the get station wash config var bool o k response
func (o *GetStationWashConfigVarBoolOK) WithPayload(payload *model.StationConfigVarBool) *GetStationWashConfigVarBoolOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get station wash config var bool o k response
func (o *GetStationWashConfigVarBoolOK) SetPayload(payload *model.StationConfigVarBool) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetStationWashConfigVarBoolOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetStationWashConfigVarBoolOK) GetStationWashConfigVarBoolResponder() {}

// GetStationWashConfigVarBoolNotFoundCode is the HTTP code returned for type GetStationWashConfigVarBoolNotFound
const GetStationWashConfigVarBoolNotFoundCode int = 404

/*
GetStationWashConfigVarBoolNotFound Not found

swagger:response getStationWashConfigVarBoolNotFound
*/
type GetStationWashConfigVarBoolNotFound struct {
}

// NewGetStationWashConfigVarBoolNotFound creates GetStationWashConfigVarBoolNotFound with default headers values
func NewGetStationWashConfigVarBoolNotFound() *GetStationWashConfigVarBoolNotFound {

	return &GetStationWashConfigVarBoolNotFound{}
}

// WriteResponse to the client
func (o *GetStationWashConfigVarBoolNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetStationWashConfigVarBoolNotFound) GetStationWashConfigVarBoolResponder() {}

// GetStationWashConfigVarBoolInternalServerErrorCode is the HTTP code returned for type GetStationWashConfigVarBoolInternalServerError
const GetStationWashConfigVarBoolInternalServerErrorCode int = 500

/*
GetStationWashConfigVarBoolInternalServerError Internal error

swagger:response getStationWashConfigVarBoolInternalServerError
*/
type GetStationWashConfigVarBoolInternalServerError struct {
}

// NewGetStationWashConfigVarBoolInternalServerError creates GetStationWashConfigVarBoolInternalServerError with default headers values
func NewGetStationWashConfigVarBoolInternalServerError() *GetStationWashConfigVarBoolInternalServerError {

	return &GetStationWashConfigVarBoolInternalServerError{}
}

// WriteResponse to the client
func (o *GetStationWashConfigVarBoolInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetStationWashConfigVarBoolInternalServerError) GetStationWashConfigVarBoolResponder() {}

type GetStationWashConfigVarBoolNotImplementedResponder struct {
	middleware.Responder
}

func (*GetStationWashConfigVarBoolNotImplementedResponder) GetStationWashConfigVarBoolResponder() {}

func GetStationWashConfigVarBoolNotImplemented() GetStationWashConfigVarBoolResponder {
	return &GetStationWashConfigVarBoolNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetStationWashConfigVarBool has not yet been implemented",
		),
	}
}

type GetStationWashConfigVarBoolResponder interface {
	middleware.Responder
	GetStationWashConfigVarBoolResponder()
}
