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

// GetListBuildScriptsOKCode is the HTTP code returned for type GetListBuildScriptsOK
const GetListBuildScriptsOKCode int = 200

/*
GetListBuildScriptsOK OK

swagger:response getListBuildScriptsOK
*/
type GetListBuildScriptsOK struct {

	/*
	  In: Body
	*/
	Payload []*model.BuildScript `json:"body,omitempty"`
}

// NewGetListBuildScriptsOK creates GetListBuildScriptsOK with default headers values
func NewGetListBuildScriptsOK() *GetListBuildScriptsOK {

	return &GetListBuildScriptsOK{}
}

// WithPayload adds the payload to the get list build scripts o k response
func (o *GetListBuildScriptsOK) WithPayload(payload []*model.BuildScript) *GetListBuildScriptsOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get list build scripts o k response
func (o *GetListBuildScriptsOK) SetPayload(payload []*model.BuildScript) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetListBuildScriptsOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if payload == nil {
		// return empty array
		payload = make([]*model.BuildScript, 0, 50)
	}

	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *GetListBuildScriptsOK) GetListBuildScriptsResponder() {}

// GetListBuildScriptsUnauthorizedCode is the HTTP code returned for type GetListBuildScriptsUnauthorized
const GetListBuildScriptsUnauthorizedCode int = 401

/*
GetListBuildScriptsUnauthorized PIN is missing or invalid

swagger:response getListBuildScriptsUnauthorized
*/
type GetListBuildScriptsUnauthorized struct {
}

// NewGetListBuildScriptsUnauthorized creates GetListBuildScriptsUnauthorized with default headers values
func NewGetListBuildScriptsUnauthorized() *GetListBuildScriptsUnauthorized {

	return &GetListBuildScriptsUnauthorized{}
}

// WriteResponse to the client
func (o *GetListBuildScriptsUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *GetListBuildScriptsUnauthorized) GetListBuildScriptsResponder() {}

// GetListBuildScriptsInternalServerErrorCode is the HTTP code returned for type GetListBuildScriptsInternalServerError
const GetListBuildScriptsInternalServerErrorCode int = 500

/*
GetListBuildScriptsInternalServerError Internal error

swagger:response getListBuildScriptsInternalServerError
*/
type GetListBuildScriptsInternalServerError struct {
}

// NewGetListBuildScriptsInternalServerError creates GetListBuildScriptsInternalServerError with default headers values
func NewGetListBuildScriptsInternalServerError() *GetListBuildScriptsInternalServerError {

	return &GetListBuildScriptsInternalServerError{}
}

// WriteResponse to the client
func (o *GetListBuildScriptsInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetListBuildScriptsInternalServerError) GetListBuildScriptsResponder() {}

type GetListBuildScriptsNotImplementedResponder struct {
	middleware.Responder
}

func (*GetListBuildScriptsNotImplementedResponder) GetListBuildScriptsResponder() {}

func GetListBuildScriptsNotImplemented() GetListBuildScriptsResponder {
	return &GetListBuildScriptsNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetListBuildScripts has not yet been implemented",
		),
	}
}

type GetListBuildScriptsResponder interface {
	middleware.Responder
	GetListBuildScriptsResponder()
}
