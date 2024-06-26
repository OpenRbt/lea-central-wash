// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	"github.com/go-openapi/runtime/middleware"

	"github.com/OpenRbt/lea-central-wash/storageapi"
)

// EditAdvertisingCampaignHandlerFunc turns a function with the right signature into a edit advertising campaign handler
type EditAdvertisingCampaignHandlerFunc func(EditAdvertisingCampaignParams, *storageapi.Profile) EditAdvertisingCampaignResponder

// Handle executing the request and returning a response
func (fn EditAdvertisingCampaignHandlerFunc) Handle(params EditAdvertisingCampaignParams, principal *storageapi.Profile) EditAdvertisingCampaignResponder {
	return fn(params, principal)
}

// EditAdvertisingCampaignHandler interface for that can handle valid edit advertising campaign params
type EditAdvertisingCampaignHandler interface {
	Handle(EditAdvertisingCampaignParams, *storageapi.Profile) EditAdvertisingCampaignResponder
}

// NewEditAdvertisingCampaign creates a new http.Handler for the edit advertising campaign operation
func NewEditAdvertisingCampaign(ctx *middleware.Context, handler EditAdvertisingCampaignHandler) *EditAdvertisingCampaign {
	return &EditAdvertisingCampaign{Context: ctx, Handler: handler}
}

/*
	EditAdvertisingCampaign swagger:route POST /edit-advertising-campaign editAdvertisingCampaign

EditAdvertisingCampaign edit advertising campaign API
*/
type EditAdvertisingCampaign struct {
	Context *middleware.Context
	Handler EditAdvertisingCampaignHandler
}

func (o *EditAdvertisingCampaign) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewEditAdvertisingCampaignParams()
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
