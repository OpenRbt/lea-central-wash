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

	strfmt "github.com/go-openapi/strfmt"
)

// NewLoadFromStationParams creates a new LoadFromStationParams object
// with the default values initialized.
func NewLoadFromStationParams() *LoadFromStationParams {
	var ()
	return &LoadFromStationParams{

		timeout: cr.DefaultTimeout,
	}
}

// NewLoadFromStationParamsWithTimeout creates a new LoadFromStationParams object
// with the default values initialized, and the ability to set a timeout on a request
func NewLoadFromStationParamsWithTimeout(timeout time.Duration) *LoadFromStationParams {
	var ()
	return &LoadFromStationParams{

		timeout: timeout,
	}
}

// NewLoadFromStationParamsWithContext creates a new LoadFromStationParams object
// with the default values initialized, and the ability to set a context for a request
func NewLoadFromStationParamsWithContext(ctx context.Context) *LoadFromStationParams {
	var ()
	return &LoadFromStationParams{

		Context: ctx,
	}
}

// NewLoadFromStationParamsWithHTTPClient creates a new LoadFromStationParams object
// with the default values initialized, and the ability to set a custom HTTPClient for a request
func NewLoadFromStationParamsWithHTTPClient(client *http.Client) *LoadFromStationParams {
	var ()
	return &LoadFromStationParams{
		HTTPClient: client,
	}
}

/*LoadFromStationParams contains all the parameters to send to the API endpoint
for the load from station operation typically these are written to a http.Request
*/
type LoadFromStationParams struct {

	/*Args*/
	Args LoadFromStationBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithTimeout adds the timeout to the load from station params
func (o *LoadFromStationParams) WithTimeout(timeout time.Duration) *LoadFromStationParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the load from station params
func (o *LoadFromStationParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the load from station params
func (o *LoadFromStationParams) WithContext(ctx context.Context) *LoadFromStationParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the load from station params
func (o *LoadFromStationParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the load from station params
func (o *LoadFromStationParams) WithHTTPClient(client *http.Client) *LoadFromStationParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the load from station params
func (o *LoadFromStationParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the load from station params
func (o *LoadFromStationParams) WithArgs(args LoadFromStationBody) *LoadFromStationParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the load from station params
func (o *LoadFromStationParams) SetArgs(args LoadFromStationBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *LoadFromStationParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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