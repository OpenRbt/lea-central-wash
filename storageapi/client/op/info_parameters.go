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

// NewInfoParams creates a new InfoParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewInfoParams() *InfoParams {
	return &InfoParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewInfoParamsWithTimeout creates a new InfoParams object
// with the ability to set a timeout on a request.
func NewInfoParamsWithTimeout(timeout time.Duration) *InfoParams {
	return &InfoParams{
		timeout: timeout,
	}
}

// NewInfoParamsWithContext creates a new InfoParams object
// with the ability to set a context for a request.
func NewInfoParamsWithContext(ctx context.Context) *InfoParams {
	return &InfoParams{
		Context: ctx,
	}
}

// NewInfoParamsWithHTTPClient creates a new InfoParams object
// with the ability to set a custom HTTPClient for a request.
func NewInfoParamsWithHTTPClient(client *http.Client) *InfoParams {
	return &InfoParams{
		HTTPClient: client,
	}
}

/*
InfoParams contains all the parameters to send to the API endpoint

	for the info operation.

	Typically these are written to a http.Request.
*/
type InfoParams struct {
	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the info params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *InfoParams) WithDefaults() *InfoParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the info params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *InfoParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the info params
func (o *InfoParams) WithTimeout(timeout time.Duration) *InfoParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the info params
func (o *InfoParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the info params
func (o *InfoParams) WithContext(ctx context.Context) *InfoParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the info params
func (o *InfoParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the info params
func (o *InfoParams) WithHTTPClient(client *http.Client) *InfoParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the info params
func (o *InfoParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WriteToRequest writes these params to a swagger request
func (o *InfoParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

	if err := r.SetTimeout(o.timeout); err != nil {
		return err
	}
	var res []error

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}
