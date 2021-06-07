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

// NewSaveMoneyParams creates a new SaveMoneyParams object,
// with the default timeout for this client.
//
// Default values are not hydrated, since defaults are normally applied by the API server side.
//
// To enforce default values in parameter, use SetDefaults or WithDefaults.
func NewSaveMoneyParams() *SaveMoneyParams {
	return &SaveMoneyParams{
		timeout: cr.DefaultTimeout,
	}
}

// NewSaveMoneyParamsWithTimeout creates a new SaveMoneyParams object
// with the ability to set a timeout on a request.
func NewSaveMoneyParamsWithTimeout(timeout time.Duration) *SaveMoneyParams {
	return &SaveMoneyParams{
		timeout: timeout,
	}
}

// NewSaveMoneyParamsWithContext creates a new SaveMoneyParams object
// with the ability to set a context for a request.
func NewSaveMoneyParamsWithContext(ctx context.Context) *SaveMoneyParams {
	return &SaveMoneyParams{
		Context: ctx,
	}
}

// NewSaveMoneyParamsWithHTTPClient creates a new SaveMoneyParams object
// with the ability to set a custom HTTPClient for a request.
func NewSaveMoneyParamsWithHTTPClient(client *http.Client) *SaveMoneyParams {
	return &SaveMoneyParams{
		HTTPClient: client,
	}
}

/* SaveMoneyParams contains all the parameters to send to the API endpoint
   for the save money operation.

   Typically these are written to a http.Request.
*/
type SaveMoneyParams struct {

	// Args.
	Args *model.MoneyReport

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithDefaults hydrates default values in the save money params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SaveMoneyParams) WithDefaults() *SaveMoneyParams {
	o.SetDefaults()
	return o
}

// SetDefaults hydrates default values in the save money params (not the query body).
//
// All values with no default are reset to their zero value.
func (o *SaveMoneyParams) SetDefaults() {
	// no default values defined for this parameter
}

// WithTimeout adds the timeout to the save money params
func (o *SaveMoneyParams) WithTimeout(timeout time.Duration) *SaveMoneyParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the save money params
func (o *SaveMoneyParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the save money params
func (o *SaveMoneyParams) WithContext(ctx context.Context) *SaveMoneyParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the save money params
func (o *SaveMoneyParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the save money params
func (o *SaveMoneyParams) WithHTTPClient(client *http.Client) *SaveMoneyParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the save money params
func (o *SaveMoneyParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the save money params
func (o *SaveMoneyParams) WithArgs(args *model.MoneyReport) *SaveMoneyParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the save money params
func (o *SaveMoneyParams) SetArgs(args *model.MoneyReport) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SaveMoneyParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
