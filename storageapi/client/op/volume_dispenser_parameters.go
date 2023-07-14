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

// NewVolumeDispenserParams creates a new VolumeDispenserParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewVolumeDispenserParams() *VolumeDispenserParams {
	return &VolumeDispenserParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewVolumeDispenserParamsWithTimeout creates a new VolumeDispenserParams object
// with the ability to set a timeout on a request.
func NewVolumeDispenserParamsWithTimeout(timeout time.Duration) *VolumeDispenserParams {
	return &VolumeDispenserParams{
		timeout: timeout,
	}
}

// NewVolumeDispenserParamsWithContext creates a new VolumeDispenserParams object
// with the ability to set a context for a request.
func NewVolumeDispenserParamsWithContext(ctx context.Context) *VolumeDispenserParams {
	return &VolumeDispenserParams{
		Context: ctx,
	}
}

// NewVolumeDispenserParamsWithHTTPClient creates a new VolumeDispenserParams object
// with the ability to set a custom HTTPClient for a request.
func NewVolumeDispenserParamsWithHTTPClient(client *http.Client) *VolumeDispenserParams {
	return &VolumeDispenserParams{
		HTTPClient: client,
	}
}

/*
VolumeDispenserParams contains all the parameters to send to the API endpoint

	for the volume dispenser operation.

	Typically these are written to a http.Request.
*/
type VolumeDispenserParams struct {

	// Args.
	Args VolumeDispenserBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the volume dispenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *VolumeDispenserParams) WithDefaults() *VolumeDispenserParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the volume dispenser params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *VolumeDispenserParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the volume dispenser params
func (o *VolumeDispenserParams) WithTimeout(timeout time.Duration) *VolumeDispenserParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the volume dispenser params
func (o *VolumeDispenserParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the volume dispenser params
func (o *VolumeDispenserParams) WithContext(ctx context.Context) *VolumeDispenserParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the volume dispenser params
func (o *VolumeDispenserParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the volume dispenser params
func (o *VolumeDispenserParams) WithHTTPClient(client *http.Client) *VolumeDispenserParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the volume dispenser params
func (o *VolumeDispenserParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the volume dispenser params
func (o *VolumeDispenserParams) WithArgs(args VolumeDispenserBody) *VolumeDispenserParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the volume dispenser params
func (o *VolumeDispenserParams) SetArgs(args VolumeDispenserBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *VolumeDispenserParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
