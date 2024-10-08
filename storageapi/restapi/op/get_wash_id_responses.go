// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// GetWashIDOKCode is the HTTP code returned for type GetWashIDOK
const GetWashIDOKCode int = 200

/*
GetWashIDOK OK

swagger:response getWashIdOK
*/
type GetWashIDOK struct {

	/*
	  In: Body
	*/
	Payload *GetWashIDOKBody `json:"body,omitempty"`
}

// NewGetWashIDOK creates GetWashIDOK with default headers values
func NewGetWashIDOK() *GetWashIDOK {

	return &GetWashIDOK{}
}

// WithPayload adds the payload to the get wash Id o k response
func (o *GetWashIDOK) WithPayload(payload *GetWashIDOKBody) *GetWashIDOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get wash Id o k response
func (o *GetWashIDOK) SetPayload(payload *GetWashIDOKBody) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetWashIDOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *GetWashIDOK) GetWashIDResponder() {}

// GetWashIDInternalServerErrorCode is the HTTP code returned for type GetWashIDInternalServerError
const GetWashIDInternalServerErrorCode int = 500

/*
GetWashIDInternalServerError Internal error

swagger:response getWashIdInternalServerError
*/
type GetWashIDInternalServerError struct {
}

// NewGetWashIDInternalServerError creates GetWashIDInternalServerError with default headers values
func NewGetWashIDInternalServerError() *GetWashIDInternalServerError {

	return &GetWashIDInternalServerError{}
}

// WriteResponse to the client
func (o *GetWashIDInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetWashIDInternalServerError) GetWashIDResponder() {}

type GetWashIDNotImplementedResponder struct {
	middleware.Responder
}

func (*GetWashIDNotImplementedResponder) GetWashIDResponder() {}

func GetWashIDNotImplemented() GetWashIDResponder {
	return &GetWashIDNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetWashID has not yet been implemented",
		),
	}
}

type GetWashIDResponder interface {
	middleware.Responder
	GetWashIDResponder()
}
