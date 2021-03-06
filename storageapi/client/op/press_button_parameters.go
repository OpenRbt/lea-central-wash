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

// NewPressButtonParams creates a new PressButtonParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewPressButtonParams() *PressButtonParams {
	return &PressButtonParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewPressButtonParamsWithTimeout creates a new PressButtonParams object
// with the ability to set a timeout on a request.
func NewPressButtonParamsWithTimeout(timeout time.Duration) *PressButtonParams {
	return &PressButtonParams{
		timeout: timeout,
	}
}

// NewPressButtonParamsWithContext creates a new PressButtonParams object
// with the ability to set a context for a request.
func NewPressButtonParamsWithContext(ctx context.Context) *PressButtonParams {
	return &PressButtonParams{
		Context: ctx,
	}
}

// NewPressButtonParamsWithHTTPClient creates a new PressButtonParams object
// with the ability to set a custom HTTPClient for a request.
func NewPressButtonParamsWithHTTPClient(client *http.Client) *PressButtonParams {
	return &PressButtonParams{
		HTTPClient: client,
	}
}

/* PressButtonParams contains all the parameters to send to the API endpoint
   for the press button operation.

   Typically these are written to a http.Request.
*/
type PressButtonParams struct {

	// Args.
	Args PressButtonBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the press button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *PressButtonParams) WithDefaults() *PressButtonParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the press button params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *PressButtonParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the press button params
func (o *PressButtonParams) WithTimeout(timeout time.Duration) *PressButtonParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the press button params
func (o *PressButtonParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the press button params
func (o *PressButtonParams) WithContext(ctx context.Context) *PressButtonParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the press button params
func (o *PressButtonParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the press button params
func (o *PressButtonParams) WithHTTPClient(client *http.Client) *PressButtonParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the press button params
func (o *PressButtonParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the press button params
func (o *PressButtonParams) WithArgs(args PressButtonBody) *PressButtonParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the press button params
func (o *PressButtonParams) SetArgs(args PressButtonBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *PressButtonParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
