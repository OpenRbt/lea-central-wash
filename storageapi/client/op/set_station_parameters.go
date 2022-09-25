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

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// NewSetStationParams creates a new SetStationParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewSetStationParams() *SetStationParams {
	return &SetStationParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewSetStationParamsWithTimeout creates a new SetStationParams object
// with the ability to set a timeout on a request.
func NewSetStationParamsWithTimeout(timeout time.Duration) *SetStationParams {
	return &SetStationParams{
		timeout: timeout,
	}
}

// NewSetStationParamsWithContext creates a new SetStationParams object
// with the ability to set a context for a request.
func NewSetStationParamsWithContext(ctx context.Context) *SetStationParams {
	return &SetStationParams{
		Context: ctx,
	}
}

// NewSetStationParamsWithHTTPClient creates a new SetStationParams object
// with the ability to set a custom HTTPClient for a request.
func NewSetStationParamsWithHTTPClient(client *http.Client) *SetStationParams {
	return &SetStationParams{
		HTTPClient: client,
	}
}

/*
SetStationParams contains all the parameters to send to the API endpoint

	for the set station operation.

	Typically these are written to a http.Request.
*/
type SetStationParams struct {

	// Args.
	Args *model.StationConfig

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the set station params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationParams) WithDefaults() *SetStationParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the set station params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the set station params
func (o *SetStationParams) WithTimeout(timeout time.Duration) *SetStationParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the set station params
func (o *SetStationParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the set station params
func (o *SetStationParams) WithContext(ctx context.Context) *SetStationParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the set station params
func (o *SetStationParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the set station params
func (o *SetStationParams) WithHTTPClient(client *http.Client) *SetStationParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the set station params
func (o *SetStationParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the set station params
func (o *SetStationParams) WithArgs(args *model.StationConfig) *SetStationParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the set station params
func (o *SetStationParams) SetArgs(args *model.StationConfig) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SetStationParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

	if err := r.SetTimeout(o.timeout); err != nil {
		return err
	}
	var res []error
	if o.Args != nil {
		if err := r.SetBodyParam(o.Args); err != nil {
			return err
		}
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}
