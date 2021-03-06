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

// NewProgramsParams creates a new ProgramsParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewProgramsParams() *ProgramsParams {
	return &ProgramsParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewProgramsParamsWithTimeout creates a new ProgramsParams object
// with the ability to set a timeout on a request.
func NewProgramsParamsWithTimeout(timeout time.Duration) *ProgramsParams {
	return &ProgramsParams{
		timeout: timeout,
	}
}

// NewProgramsParamsWithContext creates a new ProgramsParams object
// with the ability to set a context for a request.
func NewProgramsParamsWithContext(ctx context.Context) *ProgramsParams {
	return &ProgramsParams{
		Context: ctx,
	}
}

// NewProgramsParamsWithHTTPClient creates a new ProgramsParams object
// with the ability to set a custom HTTPClient for a request.
func NewProgramsParamsWithHTTPClient(client *http.Client) *ProgramsParams {
	return &ProgramsParams{
		HTTPClient: client,
	}
}

/* ProgramsParams contains all the parameters to send to the API endpoint
   for the programs operation.

   Typically these are written to a http.Request.
*/
type ProgramsParams struct {

	// Args.
	Args ProgramsBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the programs params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *ProgramsParams) WithDefaults() *ProgramsParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the programs params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *ProgramsParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the programs params
func (o *ProgramsParams) WithTimeout(timeout time.Duration) *ProgramsParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the programs params
func (o *ProgramsParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the programs params
func (o *ProgramsParams) WithContext(ctx context.Context) *ProgramsParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the programs params
func (o *ProgramsParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the programs params
func (o *ProgramsParams) WithHTTPClient(client *http.Client) *ProgramsParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the programs params
func (o *ProgramsParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the programs params
func (o *ProgramsParams) WithArgs(args ProgramsBody) *ProgramsParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the programs params
func (o *ProgramsParams) SetArgs(args ProgramsBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *ProgramsParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
