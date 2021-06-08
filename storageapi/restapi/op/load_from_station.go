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

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// LoadFromStationHandlerFunc turns a function with the right signature into a load from station handler
type LoadFromStationHandlerFunc func(LoadFromStationParams) LoadFromStationResponder

// Handle executing the request and returning a response
func (fn LoadFromStationHandlerFunc) Handle(params LoadFromStationParams) LoadFromStationResponder {
	return fn(params)
}

// LoadFromStationHandler interface for that can handle valid load from station params
type LoadFromStationHandler interface {
	Handle(LoadFromStationParams) LoadFromStationResponder
}

// NewLoadFromStation creates a new http.Handler for the load from station operation
func NewLoadFromStation(ctx *middleware.Context, handler LoadFromStationHandler) *LoadFromStation {
	return &LoadFromStation{Context: ctx, Handler: handler}
}

/* LoadFromStation swagger:route POST /load-from-station loadFromStation

LoadFromStation load from station API

*/
type LoadFromStation struct {
	Context *middleware.Context
	Handler LoadFromStationHandler
}

func (o *LoadFromStation) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewLoadFromStationParams()
	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request
	o.Context.Respond(rw, r, route.Produces, route, res)

}

// LoadFromStationBody load from station body
//
// swagger:model LoadFromStationBody
type LoadFromStationBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// key
	// Required: true
	// Min Length: 1
	Key *string `json:"key"`

	// station ID
	// Required: true
	StationID *int64 `json:"stationID"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *LoadFromStationBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// key
		// Required: true
		// Min Length: 1
		Key *string `json:"key"`

		// station ID
		// Required: true
		StationID *int64 `json:"stationID"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.Key = props.Key
	o.StationID = props.StationID
	return nil
}

// Validate validates this load from station body
func (o *LoadFromStationBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateKey(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateStationID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadFromStationBody) validateHash(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"hash", "body", o.Hash); err != nil {
		return err
	}

	if o.Hash != nil {
		if err := o.Hash.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

func (o *LoadFromStationBody) validateKey(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"key", "body", o.Key); err != nil {
		return err
	}

	if err := validate.MinLength("args"+"."+"key", "body", *o.Key, 1); err != nil {
		return err
	}

	return nil
}

func (o *LoadFromStationBody) validateStationID(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"stationID", "body", o.StationID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validate this load from station body based on the context it is used
func (o *LoadFromStationBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *LoadFromStationBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {
		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *LoadFromStationBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *LoadFromStationBody) UnmarshalBinary(b []byte) error {
	var res LoadFromStationBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
