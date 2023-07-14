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

// NewDelAdvertisingCampaignParams creates a new DelAdvertisingCampaignParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewDelAdvertisingCampaignParams() *DelAdvertisingCampaignParams {
	return &DelAdvertisingCampaignParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewDelAdvertisingCampaignParamsWithTimeout creates a new DelAdvertisingCampaignParams object
// with the ability to set a timeout on a request.
func NewDelAdvertisingCampaignParamsWithTimeout(timeout time.Duration) *DelAdvertisingCampaignParams {
	return &DelAdvertisingCampaignParams{
		timeout: timeout,
	}
}

// NewDelAdvertisingCampaignParamsWithContext creates a new DelAdvertisingCampaignParams object
// with the ability to set a context for a request.
func NewDelAdvertisingCampaignParamsWithContext(ctx context.Context) *DelAdvertisingCampaignParams {
	return &DelAdvertisingCampaignParams{
		Context: ctx,
	}
}

// NewDelAdvertisingCampaignParamsWithHTTPClient creates a new DelAdvertisingCampaignParams object
// with the ability to set a custom HTTPClient for a request.
func NewDelAdvertisingCampaignParamsWithHTTPClient(client *http.Client) *DelAdvertisingCampaignParams {
	return &DelAdvertisingCampaignParams{
		HTTPClient: client,
	}
}

/*
DelAdvertisingCampaignParams contains all the parameters to send to the API endpoint

	for the del advertising campaign operation.

	Typically these are written to a http.Request.
*/
type DelAdvertisingCampaignParams struct {

	// Args.
	Args DelAdvertisingCampaignBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the del advertising campaign params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *DelAdvertisingCampaignParams) WithDefaults() *DelAdvertisingCampaignParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the del advertising campaign params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *DelAdvertisingCampaignParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) WithTimeout(timeout time.Duration) *DelAdvertisingCampaignParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) WithContext(ctx context.Context) *DelAdvertisingCampaignParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) WithHTTPClient(client *http.Client) *DelAdvertisingCampaignParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) WithArgs(args DelAdvertisingCampaignBody) *DelAdvertisingCampaignParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the del advertising campaign params
func (o *DelAdvertisingCampaignParams) SetArgs(args DelAdvertisingCampaignBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *DelAdvertisingCampaignParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
