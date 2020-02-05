// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// InfoOKCode is the HTTP code returned for type InfoOK
const InfoOKCode int = 200

/*InfoOK OK

swagger:response infoOK
*/
type InfoOK struct {

	/*
	  In: Body
	*/
	Payload string `json:"body,omitempty"`
}

// NewInfoOK creates InfoOK with default headers values
func NewInfoOK() *InfoOK {

	return &InfoOK{}
}

// WithPayload adds the payload to the info o k response
func (o *InfoOK) WithPayload(payload string) *InfoOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the info o k response
func (o *InfoOK) SetPayload(payload string) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *InfoOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *InfoOK) InfoResponder() {}

type InfoNotImplementedResponder struct {
	middleware.Responder
}

func (*InfoNotImplementedResponder) InfoResponder() {}

func InfoNotImplemented() InfoResponder {
	return &InfoNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.Info has not yet been implemented",
		),
	}
}

type InfoResponder interface {
	middleware.Responder
	InfoResponder()
}
