// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"

	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
)

// AdvertisingCampaignHandlerFunc turns a function with the right signature into a advertising campaign handler
type AdvertisingCampaignHandlerFunc func(AdvertisingCampaignParams, *storageapi.Profile) AdvertisingCampaignResponder

// Handle executing the request and returning a response
func (fn AdvertisingCampaignHandlerFunc) Handle(params AdvertisingCampaignParams, principal *storageapi.Profile) AdvertisingCampaignResponder {
	return fn(params, principal)
}

// AdvertisingCampaignHandler interface for that can handle valid advertising campaign params
type AdvertisingCampaignHandler interface {
	Handle(AdvertisingCampaignParams, *storageapi.Profile) AdvertisingCampaignResponder
}

// NewAdvertisingCampaign creates a new http.Handler for the advertising campaign operation
func NewAdvertisingCampaign(ctx *middleware.Context, handler AdvertisingCampaignHandler) *AdvertisingCampaign {
	return &AdvertisingCampaign{Context: ctx, Handler: handler}
}

/* AdvertisingCampaign swagger:route POST /advertising-campaign advertisingCampaign

AdvertisingCampaign advertising campaign API

*/
type AdvertisingCampaign struct {
	Context *middleware.Context
	Handler AdvertisingCampaignHandler
}

func (o *AdvertisingCampaign) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewAdvertisingCampaignParams()
	uprinc, aCtx, err := o.Context.Authorize(r, route)
	if err != nil {
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}
	if aCtx != nil {
		r = aCtx
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

// AdvertisingCampaignBody ArgAdvertisingCampagin
//
// swagger:model AdvertisingCampaignBody
type AdvertisingCampaignBody struct {

	// Unix time local
	EndDate *int64 `json:"endDate,omitempty"`

	// Unix time local
	StartDate *int64 `json:"startDate,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AdvertisingCampaignBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// Unix time local
		EndDate *int64 `json:"endDate,omitempty"`

		// Unix time local
		StartDate *int64 `json:"startDate,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.EndDate = props.EndDate
	o.StartDate = props.StartDate
	return nil
}

// Validate validates this advertising campaign body
func (o *AdvertisingCampaignBody) Validate(formats strfmt.Registry) error {
	return nil
}

// ContextValidate validates this advertising campaign body based on context it is used
func (o *AdvertisingCampaignBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AdvertisingCampaignBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AdvertisingCampaignBody) UnmarshalBinary(b []byte) error {
	var res AdvertisingCampaignBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
