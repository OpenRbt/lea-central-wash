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

// GetUserOKCode is the HTTP code returned for type GetUserOK
const GetUserOKCode int = 200

/*GetUserOK OK

swagger:response getUserOK
*/
type GetUserOK struct {

	/*
	  In: Body
	*/
	Payload *model.UserConfig `json:"body,omitempty"`
}

// NewGetUserOK creates GetUserOK with default headers values
func NewGetUserOK() *GetUserOK {

	return &GetUserOK{}
}

// WithPayload adds the payload to the get user o k response
func (o *GetUserOK) WithPayload(payload *model.UserConfig) *GetUserOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get user o k response
func (o *GetUserOK) SetPayload(payload *model.UserConfig) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetUserOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetUserOK) GetUserResponder() {}

// GetUserUnauthorizedCode is the HTTP code returned for type GetUserUnauthorized
const GetUserUnauthorizedCode int = 401

/*GetUserUnauthorized PIN is missing or invalid

swagger:response getUserUnauthorized
*/
type GetUserUnauthorized struct {
}

// NewGetUserUnauthorized creates GetUserUnauthorized with default headers values
func NewGetUserUnauthorized() *GetUserUnauthorized {

	return &GetUserUnauthorized{}
}

// WriteResponse to the client
func (o *GetUserUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *GetUserUnauthorized) GetUserResponder() {}

type GetUserNotImplementedResponder struct {
	middleware.Responder
}

func (*GetUserNotImplementedResponder) GetUserResponder() {}

func GetUserNotImplemented() GetUserResponder {
	return &GetUserNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetUser has not yet been implemented",
		),
	}
}

type GetUserResponder interface {
	middleware.Responder
	GetUserResponder()
}
