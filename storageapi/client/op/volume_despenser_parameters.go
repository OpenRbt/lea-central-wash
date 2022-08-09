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

// NewVolumeDespenserParams creates a new VolumeDespenserParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewVolumeDespenserParams() *VolumeDespenserParams {
	return &VolumeDespenserParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewVolumeDespenserParamsWithTimeout creates a new VolumeDespenserParams object
// with the ability to set a timeout on a request.
func NewVolumeDespenserParamsWithTimeout(timeout time.Duration) *VolumeDespenserParams {
	return &VolumeDespenserParams{
		timeout: timeout,
	}
}

// NewVolumeDespenserParamsWithContext creates a new VolumeDespenserParams object
// with the ability to set a context for a request.
func NewVolumeDespenserParamsWithContext(ctx context.Context) *VolumeDespenserParams {
	return &VolumeDespenserParams{
		Context: ctx,
	}
}

// NewVolumeDespenserParamsWithHTTPClient creates a new VolumeDespenserParams object
// with the ability to set a custom HTTPClient for a request.
func NewVolumeDespenserParamsWithHTTPClient(client *http.Client) *VolumeDespenserParams {
	return &VolumeDespenserParams{
		HTTPClient: client,
	}
}

/* VolumeDespenserParams contains all the parameters to send to the API endpoint
   for the volume despenser operation.

   Typically these are written to a http.Request.
*/
type VolumeDespenserParams struct {

	// Args.
	Args VolumeDespenserBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the volume despenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *VolumeDespenserParams) WithDefaults() *VolumeDespenserParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the volume despenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *VolumeDespenserParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the volume despenser params
func (o *VolumeDespenserParams) WithTimeout(timeout time.Duration) *VolumeDespenserParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the volume despenser params
func (o *VolumeDespenserParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the volume despenser params
func (o *VolumeDespenserParams) WithContext(ctx context.Context) *VolumeDespenserParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the volume despenser params
func (o *VolumeDespenserParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the volume despenser params
func (o *VolumeDespenserParams) WithHTTPClient(client *http.Client) *VolumeDespenserParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the volume despenser params
func (o *VolumeDespenserParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the volume despenser params
func (o *VolumeDespenserParams) WithArgs(args VolumeDespenserBody) *VolumeDespenserParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the volume despenser params
func (o *VolumeDespenserParams) SetArgs(args VolumeDespenserBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *VolumeDespenserParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
