// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// DelStationNoContentCode is the HTTP code returned for type DelStationNoContent
const DelStationNoContentCode int = 204

/*DelStationNoContent OK

swagger:response delStationNoContent
*/
type DelStationNoContent struct {
}

// NewDelStationNoContent creates DelStationNoContent with default headers values
func NewDelStationNoContent() *DelStationNoContent {

	return &DelStationNoContent{}
}

// WriteResponse to the client
func (o *DelStationNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *DelStationNoContent) DelStationResponder() {}

// DelStationNotFoundCode is the HTTP code returned for type DelStationNotFound
const DelStationNotFoundCode int = 404

/*DelStationNotFound not found

swagger:response delStationNotFound
*/
type DelStationNotFound struct {
}

// NewDelStationNotFound creates DelStationNotFound with default headers values
func NewDelStationNotFound() *DelStationNotFound {

	return &DelStationNotFound{}
}

// WriteResponse to the client
func (o *DelStationNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *DelStationNotFound) DelStationResponder() {}

// DelStationInternalServerErrorCode is the HTTP code returned for type DelStationInternalServerError
const DelStationInternalServerErrorCode int = 500

/*DelStationInternalServerError internal error

swagger:response delStationInternalServerError
*/
type DelStationInternalServerError struct {
}

// NewDelStationInternalServerError creates DelStationInternalServerError with default headers values
func NewDelStationInternalServerError() *DelStationInternalServerError {

	return &DelStationInternalServerError{}
}

// WriteResponse to the client
func (o *DelStationInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *DelStationInternalServerError) DelStationResponder() {}

type DelStationNotImplementedResponder struct {
	middleware.Responder
}

func (*DelStationNotImplementedResponder) DelStationResponder() {}

func DelStationNotImplemented() DelStationResponder {
	return &DelStationNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.DelStation has not yet been implemented",
		),
	}
}

type DelStationResponder interface {
	middleware.Responder
	DelStationResponder()
}
