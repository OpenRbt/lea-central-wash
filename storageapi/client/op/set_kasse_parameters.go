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

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// NewSetKasseParams creates a new SetKasseParams object
// with the default values initialized.
func NewSetKasseParams() *SetKasseParams {
	var ()
	return &SetKasseParams{

		timeout: cr.DefaultTimeout,
	}
}

// NewSetKasseParamsWithTimeout creates a new SetKasseParams object
// with the default values initialized, and the ability to set a timeout on a request
func NewSetKasseParamsWithTimeout(timeout time.Duration) *SetKasseParams {
	var ()
	return &SetKasseParams{

		timeout: timeout,
	}
}

// NewSetKasseParamsWithContext creates a new SetKasseParams object
// with the default values initialized, and the ability to set a context for a request
func NewSetKasseParamsWithContext(ctx context.Context) *SetKasseParams {
	var ()
	return &SetKasseParams{

		Context: ctx,
	}
}

// NewSetKasseParamsWithHTTPClient creates a new SetKasseParams object
// with the default values initialized, and the ability to set a custom HTTPClient for a request
func NewSetKasseParamsWithHTTPClient(client *http.Client) *SetKasseParams {
	var ()
	return &SetKasseParams{
		HTTPClient: client,
	}
}

/*SetKasseParams contains all the parameters to send to the API endpoint
for the set kasse operation typically these are written to a http.Request
*/
type SetKasseParams struct {

	/*Args*/
	Args *model.KasseConfig

	timeout    time.Duration
	Context    context.Context
	HTTPClient *http.Client
}

// WithTimeout adds the timeout to the set kasse params
func (o *SetKasseParams) WithTimeout(timeout time.Duration) *SetKasseParams {
	o.SetTimeout(timeout)
	return o
}

// SetTimeout adds the timeout to the set kasse params
func (o *SetKasseParams) SetTimeout(timeout time.Duration) {
	o.timeout = timeout
}

// WithContext adds the context to the set kasse params
func (o *SetKasseParams) WithContext(ctx context.Context) *SetKasseParams {
	o.SetContext(ctx)
	return o
}

// SetContext adds the context to the set kasse params
func (o *SetKasseParams) SetContext(ctx context.Context) {
	o.Context = ctx
}

// WithHTTPClient adds the HTTPClient to the set kasse params
func (o *SetKasseParams) WithHTTPClient(client *http.Client) *SetKasseParams {
	o.SetHTTPClient(client)
	return o
}

// SetHTTPClient adds the HTTPClient to the set kasse params
func (o *SetKasseParams) SetHTTPClient(client *http.Client) {
	o.HTTPClient = client
}

// WithArgs adds the args to the set kasse params
func (o *SetKasseParams) WithArgs(args *model.KasseConfig) *SetKasseParams {
	o.SetArgs(args)
	return o
}

// SetArgs adds the args to the set kasse params
func (o *SetKasseParams) SetArgs(args *model.KasseConfig) {
	o.Args = args
}

// WriteToRequest writes these params to a swagger request
func (o *SetKasseParams) WriteToRequest(r runtime.ClientRequest, reg strfmt.Registry) error {

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
