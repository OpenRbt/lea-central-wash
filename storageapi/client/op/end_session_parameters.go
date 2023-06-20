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

// NewEndSessionParams creates a new EndSessionParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewEndSessionParams() *EndSessionParams {
	return &EndSessionParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewEndSessionParamsWithTimeout creates a new EndSessionParams object
// with the ability to set a timeout on a request.
func NewEndSessionParamsWithTimeout(timeout time.Duration) *EndSessionParams {
	return &EndSessionParams{
		timeout: timeout,
	}
}

// NewEndSessionParamsWithContext creates a new EndSessionParams object
// with the ability to set a context for a request.
func NewEndSessionParamsWithContext(ctx context.Context) *EndSessionParams {
	return &EndSessionParams{
		Context: ctx,
	}
}

// NewEndSessionParamsWithHTTPClient creates a new EndSessionParams object
// with the ability to set a custom HTTPClient for a request.
func NewEndSessionParamsWithHTTPClient(client *http.Client) *EndSessionParams {
	return &EndSessionParams{
		HTTPClient: client,
	}
}

/* EndSessionParams contains all the parameters to send to the API endpoint
   for the end session operation.

   Typically these are written to a http.Request.
*/
type EndSessionParams struct {

	// Args.
	Args EndSessionBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the end session params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *EndSessionParams) WithDefaults() *EndSessionParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the end session params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *EndSessionParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the end session params
func (o *EndSessionParams) WithTimeout(timeout time.Duration) *EndSessionParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the end session params
func (o *EndSessionParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the end session params
func (o *EndSessionParams) WithContext(ctx context.Context) *EndSessionParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the end session params
func (o *EndSessionParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the end session params
func (o *EndSessionParams) WithHTTPClient(client *http.Client) *EndSessionParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the end session params
func (o *EndSessionParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the end session params
func (o *EndSessionParams) WithArgs(args EndSessionBody) *EndSessionParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the end session params
func (o *EndSessionParams) SetArgs(args EndSessionBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *EndSessionParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
