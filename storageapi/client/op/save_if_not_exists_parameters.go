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

// NewSaveIfNotExistsParams creates a new SaveIfNotExistsParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewSaveIfNotExistsParams() *SaveIfNotExistsParams {
	return &SaveIfNotExistsParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewSaveIfNotExistsParamsWithTimeout creates a new SaveIfNotExistsParams object
// with the ability to set a timeout on a request.
func NewSaveIfNotExistsParamsWithTimeout(timeout time.Duration) *SaveIfNotExistsParams {
	return &SaveIfNotExistsParams{
		timeout: timeout,
	}
}

// NewSaveIfNotExistsParamsWithContext creates a new SaveIfNotExistsParams object
// with the ability to set a context for a request.
func NewSaveIfNotExistsParamsWithContext(ctx context.Context) *SaveIfNotExistsParams {
	return &SaveIfNotExistsParams{
		Context: ctx,
	}
}

// NewSaveIfNotExistsParamsWithHTTPClient creates a new SaveIfNotExistsParams object
// with the ability to set a custom HTTPClient for a request.
func NewSaveIfNotExistsParamsWithHTTPClient(client *http.Client) *SaveIfNotExistsParams {
	return &SaveIfNotExistsParams{
		HTTPClient: client,
	}
}

/* SaveIfNotExistsParams contains all the parameters to send to the API endpoint
   for the save if not exists operation.

   Typically these are written to a http.Request.
*/
type SaveIfNotExistsParams struct {

	// Args.
	Args SaveIfNotExistsBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the save if not exists params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SaveIfNotExistsParams) WithDefaults() *SaveIfNotExistsParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the save if not exists params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SaveIfNotExistsParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the save if not exists params
func (o *SaveIfNotExistsParams) WithTimeout(timeout time.Duration) *SaveIfNotExistsParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the save if not exists params
func (o *SaveIfNotExistsParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the save if not exists params
func (o *SaveIfNotExistsParams) WithContext(ctx context.Context) *SaveIfNotExistsParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the save if not exists params
func (o *SaveIfNotExistsParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the save if not exists params
func (o *SaveIfNotExistsParams) WithHTTPClient(client *http.Client) *SaveIfNotExistsParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the save if not exists params
func (o *SaveIfNotExistsParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the save if not exists params
func (o *SaveIfNotExistsParams) WithArgs(args SaveIfNotExistsBody) *SaveIfNotExistsParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the save if not exists params
func (o *SaveIfNotExistsParams) SetArgs(args SaveIfNotExistsBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SaveIfNotExistsParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
