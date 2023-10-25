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

// CreateSessionOKCode is the HTTP code returned for type CreateSessionOK
const CreateSessionOKCode int = 200

/*
CreateSessionOK OK

swagger:response createSessionOK
*/
type CreateSessionOK struct {

	/*
	  In: Body
	*/
	Payload *model.Session `json:"body,omitempty"`
}

// NewCreateSessionOK creates CreateSessionOK with default headers values
func NewCreateSessionOK() *CreateSessionOK {

	return &CreateSessionOK{}
}

// WithPayload adds the payload to the create session o k response
func (o *CreateSessionOK) WithPayload(payload *model.Session) *CreateSessionOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the create session o k response
func (o *CreateSessionOK) SetPayload(payload *model.Session) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *CreateSessionOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	if o.Payload != nil {
		payload := o.Payload
		if err := producer.Produce(rw, payload); err != nil {
			panic(err) // let the recovery middleware deal with this
		}
	}
}

func (o *CreateSessionOK) CreateSessionResponder() {}

// CreateSessionNotFoundCode is the HTTP code returned for type CreateSessionNotFound
const CreateSessionNotFoundCode int = 404

/*
CreateSessionNotFound hash not found

swagger:response createSessionNotFound
*/
type CreateSessionNotFound struct {
}

// NewCreateSessionNotFound creates CreateSessionNotFound with default headers values
func NewCreateSessionNotFound() *CreateSessionNotFound {

	return &CreateSessionNotFound{}
}

// WriteResponse to the client
func (o *CreateSessionNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *CreateSessionNotFound) CreateSessionResponder() {}

// CreateSessionInternalServerErrorCode is the HTTP code returned for type CreateSessionInternalServerError
const CreateSessionInternalServerErrorCode int = 500

/*
CreateSessionInternalServerError Internal error

swagger:response createSessionInternalServerError
*/
type CreateSessionInternalServerError struct {
}

// NewCreateSessionInternalServerError creates CreateSessionInternalServerError with default headers values
func NewCreateSessionInternalServerError() *CreateSessionInternalServerError {

	return &CreateSessionInternalServerError{}
}

// WriteResponse to the client
func (o *CreateSessionInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *CreateSessionInternalServerError) CreateSessionResponder() {}

type CreateSessionNotImplementedResponder struct {
	middleware.Responder
}

func (*CreateSessionNotImplementedResponder) CreateSessionResponder() {}

func CreateSessionNotImplemented() CreateSessionResponder {
	return &CreateSessionNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.CreateSession has not yet been implemented",
		),
	}
}

type CreateSessionResponder interface {
	middleware.Responder
	CreateSessionResponder()
}
