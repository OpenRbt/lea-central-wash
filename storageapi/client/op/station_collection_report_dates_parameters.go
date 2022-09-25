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

// NewStationCollectionReportDatesParams creates a new StationCollectionReportDatesParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewStationCollectionReportDatesParams() *StationCollectionReportDatesParams {
	return &StationCollectionReportDatesParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewStationCollectionReportDatesParamsWithTimeout creates a new StationCollectionReportDatesParams object
// with the ability to set a timeout on a request.
func NewStationCollectionReportDatesParamsWithTimeout(timeout time.Duration) *StationCollectionReportDatesParams {
	return &StationCollectionReportDatesParams{
		timeout: timeout,
	}
}

// NewStationCollectionReportDatesParamsWithContext creates a new StationCollectionReportDatesParams object
// with the ability to set a context for a request.
func NewStationCollectionReportDatesParamsWithContext(ctx context.Context) *StationCollectionReportDatesParams {
	return &StationCollectionReportDatesParams{
		Context: ctx,
	}
}

// NewStationCollectionReportDatesParamsWithHTTPClient creates a new StationCollectionReportDatesParams object
// with the ability to set a custom HTTPClient for a request.
func NewStationCollectionReportDatesParamsWithHTTPClient(client *http.Client) *StationCollectionReportDatesParams {
	return &StationCollectionReportDatesParams{
		HTTPClient: client,
	}
}

/*
StationCollectionReportDatesParams contains all the parameters to send to the API endpoint

	for the station collection report dates operation.

	Typically these are written to a http.Request.
*/
type StationCollectionReportDatesParams struct {

	// Args.
	Args StationCollectionReportDatesBody

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the station collection report dates params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *StationCollectionReportDatesParams) WithDefaults() *StationCollectionReportDatesParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the station collection report dates params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *StationCollectionReportDatesParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the station collection report dates params
func (o *StationCollectionReportDatesParams) WithTimeout(timeout time.Duration) *StationCollectionReportDatesParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the station collection report dates params
func (o *StationCollectionReportDatesParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the station collection report dates params
func (o *StationCollectionReportDatesParams) WithContext(ctx context.Context) *StationCollectionReportDatesParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the station collection report dates params
func (o *StationCollectionReportDatesParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the station collection report dates params
func (o *StationCollectionReportDatesParams) WithHTTPClient(client *http.Client) *StationCollectionReportDatesParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the station collection report dates params
func (o *StationCollectionReportDatesParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the station collection report dates params
func (o *StationCollectionReportDatesParams) WithArgs(args StationCollectionReportDatesBody) *StationCollectionReportDatesParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the station collection report dates params
func (o *StationCollectionReportDatesParams) SetArgs(args StationCollectionReportDatesBody) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *StationCollectionReportDatesParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
