// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// AdvertisingCampaignByIDHandlerFunc turns a function with the right signature into a advertising campaign by ID handler
type AdvertisingCampaignByIDHandlerFunc func(AdvertisingCampaignByIDParams, *storageapi.Profile) AdvertisingCampaignByIDResponder

// Handle executing the request and returning a response
func (fn AdvertisingCampaignByIDHandlerFunc) Handle(params AdvertisingCampaignByIDParams, principal *storageapi.Profile) AdvertisingCampaignByIDResponder {
	return fn(params, principal)
}

// AdvertisingCampaignByIDHandler interface for that can handle valid advertising campaign by ID params
type AdvertisingCampaignByIDHandler interface {
	Handle(AdvertisingCampaignByIDParams, *storageapi.Profile) AdvertisingCampaignByIDResponder
}

// NewAdvertisingCampaignByID creates a new http.Handler for the advertising campaign by ID operation
func NewAdvertisingCampaignByID(ctx *middleware.Context, handler AdvertisingCampaignByIDHandler) *AdvertisingCampaignByID {
	return &AdvertisingCampaignByID{Context: ctx, Handler: handler}
}

/*
	AdvertisingCampaignByID swagger:route POST /advertising-campaign-by-id advertisingCampaignById

AdvertisingCampaignByID advertising campaign by ID API
*/
type AdvertisingCampaignByID struct {
	Context *middleware.Context
	Handler AdvertisingCampaignByIDHandler
}

func (o *AdvertisingCampaignByID) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewAdvertisingCampaignByIDParams()
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

// AdvertisingCampaignByIDBody ArgAdvertisingCampaignByID
//
// swagger:model AdvertisingCampaignByIDBody
type AdvertisingCampaignByIDBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *AdvertisingCampaignByIDBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// id
		// Required: true
		ID *int64 `json:"id"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.ID = props.ID
	return nil
}

// Validate validates this advertising campaign by ID body
func (o *AdvertisingCampaignByIDBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *AdvertisingCampaignByIDBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this advertising campaign by ID body based on context it is used
func (o *AdvertisingCampaignByIDBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *AdvertisingCampaignByIDBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *AdvertisingCampaignByIDBody) UnmarshalBinary(b []byte) error {
	var res AdvertisingCampaignByIDBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
