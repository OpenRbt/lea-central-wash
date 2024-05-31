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

// GetStationFirmwareVersionsOKCode is the HTTP code returned for type GetStationFirmwareVersionsOK
const GetStationFirmwareVersionsOKCode int = 200

/*
GetStationFirmwareVersionsOK OK

swagger:response getStationFirmwareVersionsOK
*/
type GetStationFirmwareVersionsOK struct {

	/*
	  In: Body
	*/
	Payload []*model.FirmwareVersion `json:"body,omitempty"`
}

// NewGetStationFirmwareVersionsOK creates GetStationFirmwareVersionsOK with default headers values
func NewGetStationFirmwareVersionsOK() *GetStationFirmwareVersionsOK {

	return &GetStationFirmwareVersionsOK{}
}

// WithPayload adds the payload to the get station firmware versions o k response
func (o *GetStationFirmwareVersionsOK) WithPayload(payload []*model.FirmwareVersion) *GetStationFirmwareVersionsOK {
	o.Payload = payload
	return o
}

// SetPayload sets the payload to the get station firmware versions o k response
func (o *GetStationFirmwareVersionsOK) SetPayload(payload []*model.FirmwareVersion) {
	o.Payload = payload
}

// WriteResponse to the client
func (o *GetStationFirmwareVersionsOK) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.WriteHeader(200)
	payload := o.Payload
	if payload == nil {
		// return empty array
		payload = make([]*model.FirmwareVersion, 0, 50)
	}

	if err := producer.Produce(rw, payload); err != nil {
		panic(err) // let the recovery middleware deal with this
	}
}

func (o *GetStationFirmwareVersionsOK) GetStationFirmwareVersionsResponder() {}

// GetStationFirmwareVersionsUnauthorizedCode is the HTTP code returned for type GetStationFirmwareVersionsUnauthorized
const GetStationFirmwareVersionsUnauthorizedCode int = 401

/*
GetStationFirmwareVersionsUnauthorized PIN is missing or invalid

swagger:response getStationFirmwareVersionsUnauthorized
*/
type GetStationFirmwareVersionsUnauthorized struct {
}

// NewGetStationFirmwareVersionsUnauthorized creates GetStationFirmwareVersionsUnauthorized with default headers values
func NewGetStationFirmwareVersionsUnauthorized() *GetStationFirmwareVersionsUnauthorized {

	return &GetStationFirmwareVersionsUnauthorized{}
}

// WriteResponse to the client
func (o *GetStationFirmwareVersionsUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *GetStationFirmwareVersionsUnauthorized) GetStationFirmwareVersionsResponder() {}

// GetStationFirmwareVersionsNotFoundCode is the HTTP code returned for type GetStationFirmwareVersionsNotFound
const GetStationFirmwareVersionsNotFoundCode int = 404

/*
GetStationFirmwareVersionsNotFound Not found

swagger:response getStationFirmwareVersionsNotFound
*/
type GetStationFirmwareVersionsNotFound struct {
}

// NewGetStationFirmwareVersionsNotFound creates GetStationFirmwareVersionsNotFound with default headers values
func NewGetStationFirmwareVersionsNotFound() *GetStationFirmwareVersionsNotFound {

	return &GetStationFirmwareVersionsNotFound{}
}

// WriteResponse to the client
func (o *GetStationFirmwareVersionsNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *GetStationFirmwareVersionsNotFound) GetStationFirmwareVersionsResponder() {}

// GetStationFirmwareVersionsInternalServerErrorCode is the HTTP code returned for type GetStationFirmwareVersionsInternalServerError
const GetStationFirmwareVersionsInternalServerErrorCode int = 500

/*
GetStationFirmwareVersionsInternalServerError Internal error

swagger:response getStationFirmwareVersionsInternalServerError
*/
type GetStationFirmwareVersionsInternalServerError struct {
}

// NewGetStationFirmwareVersionsInternalServerError creates GetStationFirmwareVersionsInternalServerError with default headers values
func NewGetStationFirmwareVersionsInternalServerError() *GetStationFirmwareVersionsInternalServerError {

	return &GetStationFirmwareVersionsInternalServerError{}
}

// WriteResponse to the client
func (o *GetStationFirmwareVersionsInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *GetStationFirmwareVersionsInternalServerError) GetStationFirmwareVersionsResponder() {}

type GetStationFirmwareVersionsNotImplementedResponder struct {
	middleware.Responder
}

func (*GetStationFirmwareVersionsNotImplementedResponder) GetStationFirmwareVersionsResponder() {}

func GetStationFirmwareVersionsNotImplemented() GetStationFirmwareVersionsResponder {
	return &GetStationFirmwareVersionsNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.GetStationFirmwareVersions has not yet been implemented",
		),
	}
}

type GetStationFirmwareVersionsResponder interface {
	middleware.Responder
	GetStationFirmwareVersionsResponder()
}