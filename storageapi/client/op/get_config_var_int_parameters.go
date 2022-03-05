// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"context"
	"net/http"
	"time"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	cr "github.com/go-openapi/runtime/client"
	"github.com/go-openapi/strfmt"
)

// NewGetConfigVarIntParams creates a new GetConfigVarIntParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewGetConfigVarIntParams() *GetConfigVarIntParams {
	return &GetConfigVarIntParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewGetConfigVarIntParamsWithTimeout creates a new GetConfigVarIntParams object
// with the ability to set a timeout on a request.
func NewGetConfigVarIntParamsWithTimeout(timeout time.Duration) *GetConfigVarIntParams {
	return &GetConfigVarIntParams{
		timeout: timeout,
	}
}

// NewGetConfigVarIntParamsWithContext creates a new GetConfigVarIntParams object
// with the ability to set a context for a request.
func NewGetConfigVarIntParamsWithContext(ctx context.Context) *GetConfigVarIntParams {
	return &GetConfigVarIntParams{
		Context: ctx,
	}
}

// NewGetConfigVarIntParamsWithHTTPClient creates a new GetConfigVarIntParams object
// with the ability to set a custom HTTPClient for a request.
func NewGetConfigVarIntParamsWithHTTPClient(client *http.Client) *GetConfigVarIntParams {
	return &GetConfigVarIntParams{
		HTTPClient: client,
	}
}

/* GetConfigVarIntParams contains all the parameters to send to the API endpoint
   for the get config var int operation.

   Typically these are written to a http.Request.
*/
type GetConfigVarIntParams struct {

	// Args.
	Args GetConfigVarIntBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the get config var int params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetConfigVarIntParams) WithDefaults() *GetConfigVarIntParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the get config var int params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetConfigVarIntParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the get config var int params
func (o *GetConfigVarIntParams) WithTimeout(timeout time.Duration) *GetConfigVarIntParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the get config var int params
func (o *GetConfigVarIntParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the get config var int params
func (o *GetConfigVarIntParams) WithContext(ctx context.Context) *GetConfigVarIntParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the get config var int params
func (o *GetConfigVarIntParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the get config var int params
func (o *GetConfigVarIntParams) WithHTTPClient(client *http.Client) *GetConfigVarIntParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the get config var int params
func (o *GetConfigVarIntParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the get config var int params
func (o *GetConfigVarIntParams) WithArgs(args GetConfigVarIntBody) *GetConfigVarIntParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the get config var int params
func (o *GetConfigVarIntParams) SetArgs(args GetConfigVarIntBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *GetConfigVarIntParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

	if err := r.SetTimeout(o.timeout); err != nil {
		return err
	}
	var res []error
	if err := r.SetBodyParam(o.Args); err != nil {
		return err
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}