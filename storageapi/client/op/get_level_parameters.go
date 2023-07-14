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

// NewGetLevelParams creates a new GetLevelParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewGetLevelParams() *GetLevelParams {
	return &GetLevelParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewGetLevelParamsWithTimeout creates a new GetLevelParams object
// with the ability to set a timeout on a request.
func NewGetLevelParamsWithTimeout(timeout time.Duration) *GetLevelParams {
	return &GetLevelParams{
		timeout: timeout,
	}
}

// NewGetLevelParamsWithContext creates a new GetLevelParams object
// with the ability to set a context for a request.
func NewGetLevelParamsWithContext(ctx context.Context) *GetLevelParams {
	return &GetLevelParams{
		Context: ctx,
	}
}

// NewGetLevelParamsWithHTTPClient creates a new GetLevelParams object
// with the ability to set a custom HTTPClient for a request.
func NewGetLevelParamsWithHTTPClient(client *http.Client) *GetLevelParams {
	return &GetLevelParams{
		HTTPClient: client,
	}
}

/*
GetLevelParams contains all the parameters to send to the API endpoint

	for the get level operation.

	Typically these are written to a http.Request.
*/
type GetLevelParams struct {

	// Args.
	Args GetLevelBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the get level params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetLevelParams) WithDefaults() *GetLevelParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the get level params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetLevelParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the get level params
func (o *GetLevelParams) WithTimeout(timeout time.Duration) *GetLevelParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the get level params
func (o *GetLevelParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the get level params
func (o *GetLevelParams) WithContext(ctx context.Context) *GetLevelParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the get level params
func (o *GetLevelParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the get level params
func (o *GetLevelParams) WithHTTPClient(client *http.Client) *GetLevelParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the get level params
func (o *GetLevelParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the get level params
func (o *GetLevelParams) WithArgs(args GetLevelBody) *GetLevelParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the get level params
func (o *GetLevelParams) SetArgs(args GetLevelBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *GetLevelParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
