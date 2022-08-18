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

// NewRunDispenserParams creates a new RunDispenserParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewRunDispenserParams() *RunDispenserParams {
	return &RunDispenserParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewRunDispenserParamsWithTimeout creates a new RunDispenserParams object
// with the ability to set a timeout on a request.
func NewRunDispenserParamsWithTimeout(timeout time.Duration) *RunDispenserParams {
	return &RunDispenserParams{
		timeout: timeout,
	}
}

// NewRunDispenserParamsWithContext creates a new RunDispenserParams object
// with the ability to set a context for a request.
func NewRunDispenserParamsWithContext(ctx context.Context) *RunDispenserParams {
	return &RunDispenserParams{
		Context: ctx,
	}
}

// NewRunDispenserParamsWithHTTPClient creates a new RunDispenserParams object
// with the ability to set a custom HTTPClient for a request.
func NewRunDispenserParamsWithHTTPClient(client *http.Client) *RunDispenserParams {
	return &RunDispenserParams{
		HTTPClient: client,
	}
}

/* RunDispenserParams contains all the parameters to send to the API endpoint
   for the run dispenser operation.

   Typically these are written to a http.Request.
*/
type RunDispenserParams struct {

	// Args.
	Args RunDispenserBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the run dispenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *RunDispenserParams) WithDefaults() *RunDispenserParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the run dispenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *RunDispenserParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the run dispenser params
func (o *RunDispenserParams) WithTimeout(timeout time.Duration) *RunDispenserParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the run dispenser params
func (o *RunDispenserParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the run dispenser params
func (o *RunDispenserParams) WithContext(ctx context.Context) *RunDispenserParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the run dispenser params
func (o *RunDispenserParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the run dispenser params
func (o *RunDispenserParams) WithHTTPClient(client *http.Client) *RunDispenserParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the run dispenser params
func (o *RunDispenserParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the run dispenser params
func (o *RunDispenserParams) WithArgs(args RunDispenserBody) *RunDispenserParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the run dispenser params
func (o *RunDispenserParams) SetArgs(args RunDispenserBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *RunDispenserParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
