// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	middleware "github.com/go-openapi/runtime/middleware"
)

// GetUserNoContentCode is the HTTP code returned for type GetUserNoContent
const GetUserNoContentCode int = 204

/*GetUserNoContent OK

swagger:response getUserNoContent
*/
type GetUserNoContent struct {
}

// NewGetUserNoContent creates GetUserNoContent with default headers values
func NewGetUserNoContent() *GetUserNoContent {

	return &GetUserNoContent{}
}

// WriteResponse to the client
func (o *GetUserNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *GetUserNoContent) GetUserResponder() {}

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
