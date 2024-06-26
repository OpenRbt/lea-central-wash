// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"net/http"

	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
)

// DeleteBuildScriptNoContentCode is the HTTP code returned for type DeleteBuildScriptNoContent
const DeleteBuildScriptNoContentCode int = 204

/*
DeleteBuildScriptNoContent OK

swagger:response deleteBuildScriptNoContent
*/
type DeleteBuildScriptNoContent struct {
}

// NewDeleteBuildScriptNoContent creates DeleteBuildScriptNoContent with default headers values
func NewDeleteBuildScriptNoContent() *DeleteBuildScriptNoContent {

	return &DeleteBuildScriptNoContent{}
}

// WriteResponse to the client
func (o *DeleteBuildScriptNoContent) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(204)
}

func (o *DeleteBuildScriptNoContent) DeleteBuildScriptResponder() {}

// DeleteBuildScriptUnauthorizedCode is the HTTP code returned for type DeleteBuildScriptUnauthorized
const DeleteBuildScriptUnauthorizedCode int = 401

/*
DeleteBuildScriptUnauthorized PIN is missing or invalid

swagger:response deleteBuildScriptUnauthorized
*/
type DeleteBuildScriptUnauthorized struct {
}

// NewDeleteBuildScriptUnauthorized creates DeleteBuildScriptUnauthorized with default headers values
func NewDeleteBuildScriptUnauthorized() *DeleteBuildScriptUnauthorized {

	return &DeleteBuildScriptUnauthorized{}
}

// WriteResponse to the client
func (o *DeleteBuildScriptUnauthorized) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(401)
}

func (o *DeleteBuildScriptUnauthorized) DeleteBuildScriptResponder() {}

// DeleteBuildScriptNotFoundCode is the HTTP code returned for type DeleteBuildScriptNotFound
const DeleteBuildScriptNotFoundCode int = 404

/*
DeleteBuildScriptNotFound Not found

swagger:response deleteBuildScriptNotFound
*/
type DeleteBuildScriptNotFound struct {
}

// NewDeleteBuildScriptNotFound creates DeleteBuildScriptNotFound with default headers values
func NewDeleteBuildScriptNotFound() *DeleteBuildScriptNotFound {

	return &DeleteBuildScriptNotFound{}
}

// WriteResponse to the client
func (o *DeleteBuildScriptNotFound) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(404)
}

func (o *DeleteBuildScriptNotFound) DeleteBuildScriptResponder() {}

// DeleteBuildScriptInternalServerErrorCode is the HTTP code returned for type DeleteBuildScriptInternalServerError
const DeleteBuildScriptInternalServerErrorCode int = 500

/*
DeleteBuildScriptInternalServerError Internal error

swagger:response deleteBuildScriptInternalServerError
*/
type DeleteBuildScriptInternalServerError struct {
}

// NewDeleteBuildScriptInternalServerError creates DeleteBuildScriptInternalServerError with default headers values
func NewDeleteBuildScriptInternalServerError() *DeleteBuildScriptInternalServerError {

	return &DeleteBuildScriptInternalServerError{}
}

// WriteResponse to the client
func (o *DeleteBuildScriptInternalServerError) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {

	rw.Header().Del(runtime.HeaderContentType) //Remove Content-Type on empty responses

	rw.WriteHeader(500)
}

func (o *DeleteBuildScriptInternalServerError) DeleteBuildScriptResponder() {}

type DeleteBuildScriptNotImplementedResponder struct {
	middleware.Responder
}

func (*DeleteBuildScriptNotImplementedResponder) DeleteBuildScriptResponder() {}

func DeleteBuildScriptNotImplemented() DeleteBuildScriptResponder {
	return &DeleteBuildScriptNotImplementedResponder{
		middleware.NotImplemented(
			"operation authentication.DeleteBuildScript has not yet been implemented",
		),
	}
}

type DeleteBuildScriptResponder interface {
	middleware.Responder
	DeleteBuildScriptResponder()
}
