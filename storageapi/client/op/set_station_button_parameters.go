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

// NewSetStationButtonParams creates a new SetStationButtonParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewSetStationButtonParams() *SetStationButtonParams {
	return &SetStationButtonParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewSetStationButtonParamsWithTimeout creates a new SetStationButtonParams object
// with the ability to set a timeout on a request.
func NewSetStationButtonParamsWithTimeout(timeout time.Duration) *SetStationButtonParams {
	return &SetStationButtonParams{
		timeout: timeout,
	}
}

// NewSetStationButtonParamsWithContext creates a new SetStationButtonParams object
// with the ability to set a context for a request.
func NewSetStationButtonParamsWithContext(ctx context.Context) *SetStationButtonParams {
	return &SetStationButtonParams{
		Context: ctx,
	}
}

// NewSetStationButtonParamsWithHTTPClient creates a new SetStationButtonParams object
// with the ability to set a custom HTTPClient for a request.
func NewSetStationButtonParamsWithHTTPClient(client *http.Client) *SetStationButtonParams {
	return &SetStationButtonParams{
		HTTPClient: client,
	}
}

/* SetStationButtonParams contains all the parameters to send to the API endpoint
   for the set station button operation.

   Typically these are written to a http.Request.
*/
type SetStationButtonParams struct {

	// Args.
	Args SetStationButtonBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the set station button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationButtonParams) WithDefaults() *SetStationButtonParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the set station button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationButtonParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the set station button params
func (o *SetStationButtonParams) WithTimeout(timeout time.Duration) *SetStationButtonParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the set station button params
func (o *SetStationButtonParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the set station button params
func (o *SetStationButtonParams) WithContext(ctx context.Context) *SetStationButtonParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the set station button params
func (o *SetStationButtonParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the set station button params
func (o *SetStationButtonParams) WithHTTPClient(client *http.Client) *SetStationButtonParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the set station button params
func (o *SetStationButtonParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the set station button params
func (o *SetStationButtonParams) WithArgs(args SetStationButtonBody) *SetStationButtonParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the set station button params
func (o *SetStationButtonParams) SetArgs(args SetStationButtonBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SetStationButtonParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
