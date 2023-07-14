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

// NewOpenStationParams creates a new OpenStationParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewOpenStationParams() *OpenStationParams {
	return &OpenStationParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewOpenStationParamsWithTimeout creates a new OpenStationParams object
// with the ability to set a timeout on a request.
func NewOpenStationParamsWithTimeout(timeout time.Duration) *OpenStationParams {
	return &OpenStationParams{
		timeout: timeout,
	}
}

// NewOpenStationParamsWithContext creates a new OpenStationParams object
// with the ability to set a context for a request.
func NewOpenStationParamsWithContext(ctx context.Context) *OpenStationParams {
	return &OpenStationParams{
		Context: ctx,
	}
}

// NewOpenStationParamsWithHTTPClient creates a new OpenStationParams object
// with the ability to set a custom HTTPClient for a request.
func NewOpenStationParamsWithHTTPClient(client *http.Client) *OpenStationParams {
	return &OpenStationParams{
		HTTPClient: client,
	}
}

/*
OpenStationParams contains all the parameters to send to the API endpoint

	for the open station operation.

	Typically these are written to a http.Request.
*/
type OpenStationParams struct {

	// Args.
	Args OpenStationBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the open station params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *OpenStationParams) WithDefaults() *OpenStationParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the open station params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *OpenStationParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the open station params
func (o *OpenStationParams) WithTimeout(timeout time.Duration) *OpenStationParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the open station params
func (o *OpenStationParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the open station params
func (o *OpenStationParams) WithContext(ctx context.Context) *OpenStationParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the open station params
func (o *OpenStationParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the open station params
func (o *OpenStationParams) WithHTTPClient(client *http.Client) *OpenStationParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the open station params
func (o *OpenStationParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the open station params
func (o *OpenStationParams) WithArgs(args OpenStationBody) *OpenStationParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the open station params
func (o *OpenStationParams) SetArgs(args OpenStationBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *OpenStationParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
