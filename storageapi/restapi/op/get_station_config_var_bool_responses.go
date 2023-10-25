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

// GetStationConfigVarBoolOKCode is the HTTP code returned for type GetStationConfigVarBoolOK
const GetStationConfigVarBoolOKCode int = 200

/*
GetStationConfigVarBoolOK OK

swagger:response getStationConfigVarBoolOK
*/
type GetStationConfigVarBoolOK struct {

	/*
	  In: Body
	*/
	Payload *model.StationConfigVarBool `json:"body,omitempty"`
}

// NewGetStationConfigVarBoolOK creates GetStationConfigVarBoolOK with default headers values
func NewGetStationConfigVarBoolOK() *GetStationConfigVarBoolOK {

	return &GetStationConfigVarBoolOK{}
}

// WithPayload adds the payload to the get station config var bool o k response
func (o *GetStationConfigVarBoolOK) WithPayload(payload *model.StationConfigVarBool) *GetStationConfigVarBoolOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get station config var bool o k response
func (o *GetStationConfigVarBoolOK) SetPayload(payload *model.StationConfigVarBool) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetStationConfigVarBoolOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetStationConfigVarBoolOK) GetStationConfigVarBoolResponder() {}

// GetStationConfigVarBoolNotFoundCode is the HTTP code returned for type GetStationConfigVarBoolNotFound
const GetStationConfigVarBoolNotFoundCode int = 404

/*
GetStationConfigVarBoolNotFound Not found

swagger:response getStationConfigVarBoolNotFound
*/
type GetStationConfigVarBoolNotFound struct {
}

// NewGetStationConfigVarBoolNotFound creates GetStationConfigVarBoolNotFound with default headers values
func NewGetStationConfigVarBoolNotFound() *GetStationConfigVarBoolNotFound {

	return &GetStationConfigVarBoolNotFound{}
}

// WriteResponse to the client
func (o *GetStationConfigVarBoolNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetStationConfigVarBoolNotFound) GetStationConfigVarBoolResponder() {}

// GetStationConfigVarBoolInternalServerErrorCode is the HTTP code returned for type GetStationConfigVarBoolInternalServerError
const GetStationConfigVarBoolInternalServerErrorCode int = 500

/*
GetStationConfigVarBoolInternalServerError Internal error

swagger:response getStationConfigVarBoolInternalServerError
*/
type GetStationConfigVarBoolInternalServerError struct {
}

// NewGetStationConfigVarBoolInternalServerError creates GetStationConfigVarBoolInternalServerError with default headers values
func NewGetStationConfigVarBoolInternalServerError() *GetStationConfigVarBoolInternalServerError {

	return &GetStationConfigVarBoolInternalServerError{}
}

// WriteResponse to the client
func (o *GetStationConfigVarBoolInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetStationConfigVarBoolInternalServerError) GetStationConfigVarBoolResponder() {}

type GetStationConfigVarBoolNotImplementedResponder struct {
	middleware.Responder
}

func (*GetStationConfigVarBoolNotImplementedResponder) GetStationConfigVarBoolResponder() {}

func GetStationConfigVarBoolNotImplemented() GetStationConfigVarBoolResponder {
	return &GetStationConfigVarBoolNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetStationConfigVarBool has not yet been implemented",
		),
	}
}

type GetStationConfigVarBoolResponder interface {
	middleware.Responder
	GetStationConfigVarBoolResponder()
}
