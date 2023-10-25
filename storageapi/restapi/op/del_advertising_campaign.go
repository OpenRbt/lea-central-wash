// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"context"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// DelAdvertisingCampaignHandlerFunc turns a function with the right signature into a del advertising campaign handler
type DelAdvertisingCampaignHandlerFunc func(DelAdvertisingCampaignParams, *storageapi.Profile) DelAdvertisingCampaignResponder

// Handle executing the request and returning a response
func (fn DelAdvertisingCampaignHandlerFunc) Handle(params DelAdvertisingCampaignParams, principal *storageapi.Profile) DelAdvertisingCampaignResponder {
	return fn(params, principal)
}

// DelAdvertisingCampaignHandler interface for that can handle valid del advertising campaign params
type DelAdvertisingCampaignHandler interface {
	Handle(DelAdvertisingCampaignParams, *storageapi.Profile) DelAdvertisingCampaignResponder
}

// NewDelAdvertisingCampaign creates a new http.Handler for the del advertising campaign operation
func NewDelAdvertisingCampaign(ctx *middleware.Context, handler DelAdvertisingCampaignHandler) *DelAdvertisingCampaign {
	return &DelAdvertisingCampaign{Context: ctx, Handler: handler}
}

/*
	DelAdvertisingCampaign swagger:route POST /del-advertising-campaign delAdvertisingCampaign

DelAdvertisingCampaign del advertising campaign API
*/
type DelAdvertisingCampaign struct {
	Context *middleware.Context
	Handler DelAdvertisingCampaignHandler
}

func (o *DelAdvertisingCampaign) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewDelAdvertisingCampaignParams()
	uprinc, aCtx, err := o.Context.Authorize(r, route)
	if err != nil {
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}
	if aCtx != nil {
		*r = *aCtx
	}
	var principal *storageapi.Profile
	if uprinc != nil {
		principal = uprinc.(*storageapi.Profile) // this is really a storageapi.Profile, I promise
	}

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params, principal) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// DelAdvertisingCampaignBody ArgDelAdvertisingCampagin
//
// swagger:model DelAdvertisingCampaignBody
type DelAdvertisingCampaignBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// Validate validates this del advertising campaign body
func (o *DelAdvertisingCampaignBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DelAdvertisingCampaignBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this del advertising campaign body based on context it is used
func (o *DelAdvertisingCampaignBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *DelAdvertisingCampaignBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DelAdvertisingCampaignBody) UnmarshalBinary(b []byte) error {
	var res DelAdvertisingCampaignBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
