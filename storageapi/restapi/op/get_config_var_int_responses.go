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

// GetConfigVarIntOKCode is the HTTP code returned for type GetConfigVarIntOK
const GetConfigVarIntOKCode int = 200

/*
GetConfigVarIntOK OK

swagger:response getConfigVarIntOK
*/
type GetConfigVarIntOK struct {

	/*
	  In: Body
	*/
	Payload *model.ConfigVarInt `json:"body,omitempty"`
}

// NewGetConfigVarIntOK creates GetConfigVarIntOK with default headers values
func NewGetConfigVarIntOK() *GetConfigVarIntOK {

	return &GetConfigVarIntOK{}
}

// WithPayload adds the payload to the get config var int o k response
func (o *GetConfigVarIntOK) WithPayload(payload *model.ConfigVarInt) *GetConfigVarIntOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get config var int o k response
func (o *GetConfigVarIntOK) SetPayload(payload *model.ConfigVarInt) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetConfigVarIntOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetConfigVarIntOK) GetConfigVarIntResponder() {}

// GetConfigVarIntNotFoundCode is the HTTP code returned for type GetConfigVarIntNotFound
const GetConfigVarIntNotFoundCode int = 404

/*
GetConfigVarIntNotFound Not found

swagger:response getConfigVarIntNotFound
*/
type GetConfigVarIntNotFound struct {
}

// NewGetConfigVarIntNotFound creates GetConfigVarIntNotFound with default headers values
func NewGetConfigVarIntNotFound() *GetConfigVarIntNotFound {

	return &GetConfigVarIntNotFound{}
}

// WriteResponse to the client
func (o *GetConfigVarIntNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetConfigVarIntNotFound) GetConfigVarIntResponder() {}

// GetConfigVarIntInternalServerErrorCode is the HTTP code returned for type GetConfigVarIntInternalServerError
const GetConfigVarIntInternalServerErrorCode int = 500

/*
GetConfigVarIntInternalServerError Internal error

swagger:response getConfigVarIntInternalServerError
*/
type GetConfigVarIntInternalServerError struct {
}

// NewGetConfigVarIntInternalServerError creates GetConfigVarIntInternalServerError with default headers values
func NewGetConfigVarIntInternalServerError() *GetConfigVarIntInternalServerError {

	return &GetConfigVarIntInternalServerError{}
}

// WriteResponse to the client
func (o *GetConfigVarIntInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetConfigVarIntInternalServerError) GetConfigVarIntResponder() {}

type GetConfigVarIntNotImplementedResponder struct {
	middleware.Responder
}

func (*GetConfigVarIntNotImplementedResponder) GetConfigVarIntResponder() {}

func GetConfigVarIntNotImplemented() GetConfigVarIntResponder {
	return &GetConfigVarIntNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetConfigVarInt has not yet been implemented",
		),
	}
}

type GetConfigVarIntResponder interface {
	middleware.Responder
	GetConfigVarIntResponder()
}
