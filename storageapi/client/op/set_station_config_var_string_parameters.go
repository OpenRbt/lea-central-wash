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

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// NewSetStationConfigVarStringParams creates a new SetStationConfigVarStringParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewSetStationConfigVarStringParams() *SetStationConfigVarStringParams {
	return &SetStationConfigVarStringParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewSetStationConfigVarStringParamsWithTimeout creates a new SetStationConfigVarStringParams object
// with the ability to set a timeout on a request.
func NewSetStationConfigVarStringParamsWithTimeout(timeout time.Duration) *SetStationConfigVarStringParams {
	return &SetStationConfigVarStringParams{
		timeout: timeout,
	}
}

// NewSetStationConfigVarStringParamsWithContext creates a new SetStationConfigVarStringParams object
// with the ability to set a context for a request.
func NewSetStationConfigVarStringParamsWithContext(ctx context.Context) *SetStationConfigVarStringParams {
	return &SetStationConfigVarStringParams{
		Context: ctx,
	}
}

// NewSetStationConfigVarStringParamsWithHTTPClient creates a new SetStationConfigVarStringParams object
// with the ability to set a custom HTTPClient for a request.
func NewSetStationConfigVarStringParamsWithHTTPClient(client *http.Client) *SetStationConfigVarStringParams {
	return &SetStationConfigVarStringParams{
		HTTPClient: client,
	}
}

/*
SetStationConfigVarStringParams contains all the parameters to send to the API endpoint

	for the set station config var string operation.

	Typically these are written to a http.Request.
*/
type SetStationConfigVarStringParams struct {

	// Args.
	Args *model.StationConfigVarString

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the set station config var string params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationConfigVarStringParams) WithDefaults() *SetStationConfigVarStringParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the set station config var string params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SetStationConfigVarStringParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the set station config var string params
func (o *SetStationConfigVarStringParams) WithTimeout(timeout time.Duration) *SetStationConfigVarStringParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the set station config var string params
func (o *SetStationConfigVarStringParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the set station config var string params
func (o *SetStationConfigVarStringParams) WithContext(ctx context.Context) *SetStationConfigVarStringParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the set station config var string params
func (o *SetStationConfigVarStringParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the set station config var string params
func (o *SetStationConfigVarStringParams) WithHTTPClient(client *http.Client) *SetStationConfigVarStringParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the set station config var string params
func (o *SetStationConfigVarStringParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the set station config var string params
func (o *SetStationConfigVarStringParams) WithArgs(args *model.StationConfigVarString) *SetStationConfigVarStringParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the set station config var string params
func (o *SetStationConfigVarStringParams) SetArgs(args *model.StationConfigVarString) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SetStationConfigVarStringParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
