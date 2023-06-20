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

// NewStationButtonParams creates a new StationButtonParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewStationButtonParams() *StationButtonParams {
	return &StationButtonParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewStationButtonParamsWithTimeout creates a new StationButtonParams object
// with the ability to set a timeout on a request.
func NewStationButtonParamsWithTimeout(timeout time.Duration) *StationButtonParams {
	return &StationButtonParams{
		timeout: timeout,
	}
}

// NewStationButtonParamsWithContext creates a new StationButtonParams object
// with the ability to set a context for a request.
func NewStationButtonParamsWithContext(ctx context.Context) *StationButtonParams {
	return &StationButtonParams{
		Context: ctx,
	}
}

// NewStationButtonParamsWithHTTPClient creates a new StationButtonParams object
// with the ability to set a custom HTTPClient for a request.
func NewStationButtonParamsWithHTTPClient(client *http.Client) *StationButtonParams {
	return &StationButtonParams{
		HTTPClient: client,
	}
}

/* StationButtonParams contains all the parameters to send to the API endpoint
   for the station button operation.

   Typically these are written to a http.Request.
*/
type StationButtonParams struct {

	// Args.
	Args StationButtonBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the station button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *StationButtonParams) WithDefaults() *StationButtonParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the station button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *StationButtonParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the station button params
func (o *StationButtonParams) WithTimeout(timeout time.Duration) *StationButtonParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the station button params
func (o *StationButtonParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the station button params
func (o *StationButtonParams) WithContext(ctx context.Context) *StationButtonParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the station button params
func (o *StationButtonParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the station button params
func (o *StationButtonParams) WithHTTPClient(client *http.Client) *StationButtonParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the station button params
func (o *StationButtonParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the station button params
func (o *StationButtonParams) WithArgs(args StationButtonBody) *StationButtonParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the station button params
func (o *StationButtonParams) SetArgs(args StationButtonBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *StationButtonParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
