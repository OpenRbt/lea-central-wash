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

// NewPayParams creates a new PayParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewPayParams() *PayParams {
	return &PayParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewPayParamsWithTimeout creates a new PayParams object
// with the ability to set a timeout on a request.
func NewPayParamsWithTimeout(timeout time.Duration) *PayParams {
	return &PayParams{
		timeout: timeout,
	}
}

// NewPayParamsWithContext creates a new PayParams object
// with the ability to set a context for a request.
func NewPayParamsWithContext(ctx context.Context) *PayParams {
	return &PayParams{
		Context: ctx,
	}
}

// NewPayParamsWithHTTPClient creates a new PayParams object
// with the ability to set a custom HTTPClient for a request.
func NewPayParamsWithHTTPClient(client *http.Client) *PayParams {
	return &PayParams{
		HTTPClient: client,
	}
}

/*
PayParams contains all the parameters to send to the API endpoint

	for the pay operation.

	Typically these are written to a http.Request.
*/
type PayParams struct {

	// Args.
	Args PayBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the pay params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *PayParams) WithDefaults() *PayParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the pay params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *PayParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the pay params
func (o *PayParams) WithTimeout(timeout time.Duration) *PayParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the pay params
func (o *PayParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the pay params
func (o *PayParams) WithContext(ctx context.Context) *PayParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the pay params
func (o *PayParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the pay params
func (o *PayParams) WithHTTPClient(client *http.Client) *PayParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the pay params
func (o *PayParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the pay params
func (o *PayParams) WithArgs(args PayBody) *PayParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the pay params
func (o *PayParams) SetArgs(args PayBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *PayParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
