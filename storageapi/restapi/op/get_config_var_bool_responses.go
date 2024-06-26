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

// GetConfigVarBoolOKCode is the HTTP code returned for type GetConfigVarBoolOK
const GetConfigVarBoolOKCode int = 200

/*
GetConfigVarBoolOK OK

swagger:response getConfigVarBoolOK
*/
type GetConfigVarBoolOK struct {

	/*
	  In: Body
	*/
	Payload *model.ConfigVarBool `json:"body,omitempty"`
}

// NewGetConfigVarBoolOK creates GetConfigVarBoolOK with default headers values
func NewGetConfigVarBoolOK() *GetConfigVarBoolOK {

	return &GetConfigVarBoolOK{}
}

// WithPayload adds the payload to the get config var bool o k response
func (o *GetConfigVarBoolOK) WithPayload(payload *model.ConfigVarBool) *GetConfigVarBoolOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get config var bool o k response
func (o *GetConfigVarBoolOK) SetPayload(payload *model.ConfigVarBool) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetConfigVarBoolOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetConfigVarBoolOK) GetConfigVarBoolResponder() {}

// GetConfigVarBoolNotFoundCode is the HTTP code returned for type GetConfigVarBoolNotFound
const GetConfigVarBoolNotFoundCode int = 404

/*
GetConfigVarBoolNotFound Not found

swagger:response getConfigVarBoolNotFound
*/
type GetConfigVarBoolNotFound struct {
}

// NewGetConfigVarBoolNotFound creates GetConfigVarBoolNotFound with default headers values
func NewGetConfigVarBoolNotFound() *GetConfigVarBoolNotFound {

	return &GetConfigVarBoolNotFound{}
}

// WriteResponse to the client
func (o *GetConfigVarBoolNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetConfigVarBoolNotFound) GetConfigVarBoolResponder() {}

// GetConfigVarBoolInternalServerErrorCode is the HTTP code returned for type GetConfigVarBoolInternalServerError
const GetConfigVarBoolInternalServerErrorCode int = 500

/*
GetConfigVarBoolInternalServerError Internal error

swagger:response getConfigVarBoolInternalServerError
*/
type GetConfigVarBoolInternalServerError struct {
}

// NewGetConfigVarBoolInternalServerError creates GetConfigVarBoolInternalServerError with default headers values
func NewGetConfigVarBoolInternalServerError() *GetConfigVarBoolInternalServerError {

	return &GetConfigVarBoolInternalServerError{}
}

// WriteResponse to the client
func (o *GetConfigVarBoolInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetConfigVarBoolInternalServerError) GetConfigVarBoolResponder() {}

type GetConfigVarBoolNotImplementedResponder struct {
	middleware.Responder
}

func (*GetConfigVarBoolNotImplementedResponder) GetConfigVarBoolResponder() {}

func GetConfigVarBoolNotImplemented() GetConfigVarBoolResponder {
	return &GetConfigVarBoolNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetConfigVarBool has not yet been implemented",
		),
	}
}

type GetConfigVarBoolResponder interface {
	middleware.Responder
	GetConfigVarBoolResponder()
}
