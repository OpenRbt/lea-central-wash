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

// NewGetStationConfigVarStringParams creates a new GetStationConfigVarStringParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewGetStationConfigVarStringParams() *GetStationConfigVarStringParams {
	return &GetStationConfigVarStringParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewGetStationConfigVarStringParamsWithTimeout creates a new GetStationConfigVarStringParams object
// with the ability to set a timeout on a request.
func NewGetStationConfigVarStringParamsWithTimeout(timeout time.Duration) *GetStationConfigVarStringParams {
	return &GetStationConfigVarStringParams{
		timeout: timeout,
	}
}

// NewGetStationConfigVarStringParamsWithContext creates a new GetStationConfigVarStringParams object
// with the ability to set a context for a request.
func NewGetStationConfigVarStringParamsWithContext(ctx context.Context) *GetStationConfigVarStringParams {
	return &GetStationConfigVarStringParams{
		Context: ctx,
	}
}

// NewGetStationConfigVarStringParamsWithHTTPClient creates a new GetStationConfigVarStringParams object
// with the ability to set a custom HTTPClient for a request.
func NewGetStationConfigVarStringParamsWithHTTPClient(client *http.Client) *GetStationConfigVarStringParams {
	return &GetStationConfigVarStringParams{
		HTTPClient: client,
	}
}

/*
GetStationConfigVarStringParams contains all the parameters to send to the API endpoint

	for the get station config var string operation.

	Typically these are written to a http.Request.
*/
type GetStationConfigVarStringParams struct {

	// Args.
	Args GetStationConfigVarStringBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the get station config var string params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetStationConfigVarStringParams) WithDefaults() *GetStationConfigVarStringParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the get station config var string params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *GetStationConfigVarStringParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the get station config var string params
func (o *GetStationConfigVarStringParams) WithTimeout(timeout time.Duration) *GetStationConfigVarStringParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the get station config var string params
func (o *GetStationConfigVarStringParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the get station config var string params
func (o *GetStationConfigVarStringParams) WithContext(ctx context.Context) *GetStationConfigVarStringParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the get station config var string params
func (o *GetStationConfigVarStringParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the get station config var string params
func (o *GetStationConfigVarStringParams) WithHTTPClient(client *http.Client) *GetStationConfigVarStringParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the get station config var string params
func (o *GetStationConfigVarStringParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the get station config var string params
func (o *GetStationConfigVarStringParams) WithArgs(args GetStationConfigVarStringBody) *GetStationConfigVarStringParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the get station config var string params
func (o *GetStationConfigVarStringParams) SetArgs(args GetStationConfigVarStringBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *GetStationConfigVarStringParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
