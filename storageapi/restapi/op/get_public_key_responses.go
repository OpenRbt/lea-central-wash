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

// GetPublicKeyOKCode is the HTTP code returned for type GetPublicKeyOK
const GetPublicKeyOKCode int = 200

/*
GetPublicKeyOK OK

swagger:response getPublicKeyOK
*/
type GetPublicKeyOK struct {

	/*
	  In: Body
	*/
	Payload *model.PublicKey `json:"body,omitempty"`
}

// NewGetPublicKeyOK creates GetPublicKeyOK with default headers values
func NewGetPublicKeyOK() *GetPublicKeyOK {

	return &GetPublicKeyOK{}
}

// WithPayload adds the payload to the get public key o k response
func (o *GetPublicKeyOK) WithPayload(payload *model.PublicKey) *GetPublicKeyOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get public key o k response
func (o *GetPublicKeyOK) SetPayload(payload *model.PublicKey) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetPublicKeyOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetPublicKeyOK) GetPublicKeyResponder() {}

// GetPublicKeyInternalServerErrorCode is the HTTP code returned for type GetPublicKeyInternalServerError
const GetPublicKeyInternalServerErrorCode int = 500

/*
GetPublicKeyInternalServerError Internal error

swagger:response getPublicKeyInternalServerError
*/
type GetPublicKeyInternalServerError struct {
}

// NewGetPublicKeyInternalServerError creates GetPublicKeyInternalServerError with default headers values
func NewGetPublicKeyInternalServerError() *GetPublicKeyInternalServerError {

	return &GetPublicKeyInternalServerError{}
}

// WriteResponse to the client
func (o *GetPublicKeyInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetPublicKeyInternalServerError) GetPublicKeyResponder() {}

type GetPublicKeyNotImplementedResponder struct {
	middleware.Responder
}

func (*GetPublicKeyNotImplementedResponder) GetPublicKeyResponder() {}

func GetPublicKeyNotImplemented() GetPublicKeyResponder {
	return &GetPublicKeyNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetPublicKey has not yet been implemented",
		),
	}
}

type GetPublicKeyResponder interface {
	middleware.Responder
	GetPublicKeyResponder()
}
