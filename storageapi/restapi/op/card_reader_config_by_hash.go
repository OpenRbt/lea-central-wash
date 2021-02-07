// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the generate command

import (
	"net/http"

	errors "github.com/go-openapi/errors"
	middleware "github.com/go-openapi/runtime/middleware"
	strfmt "github.com/go-openapi/strfmt"
	swag "github.com/go-openapi/swag"

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// CardReaderConfigByHashHandlerFunc turns a function with the right signature into a card reader config by hash handler
type CardReaderConfigByHashHandlerFunc func(CardReaderConfigByHashParams) CardReaderConfigByHashResponder

// Handle executing the request and returning a response
func (fn CardReaderConfigByHashHandlerFunc) Handle(params CardReaderConfigByHashParams) CardReaderConfigByHashResponder {
	return fn(params)
}

// CardReaderConfigByHashHandler interface for that can handle valid card reader config by hash params
type CardReaderConfigByHashHandler interface {
	Handle(CardReaderConfigByHashParams) CardReaderConfigByHashResponder
}

// NewCardReaderConfigByHash creates a new http.Handler for the card reader config by hash operation
func NewCardReaderConfigByHash(ctx *middleware.Context, handler CardReaderConfigByHashHandler) *CardReaderConfigByHash {
	return &CardReaderConfigByHash{Context: ctx, Handler: handler}
}

/*CardReaderConfigByHash swagger:route POST /card-reader-config-by-hash cardReaderConfigByHash

CardReaderConfigByHash card reader config by hash API

*/
type CardReaderConfigByHash struct {
	Context *middleware.Context
	Handler CardReaderConfigByHashHandler
}

func (o *CardReaderConfigByHash) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewCardReaderConfigByHashParams()

	if err := o.Context.BindValidRequest(r, route, &Params); err != nil { // bind params
		o.Context.Respond(rw, r, route.Produces, route, err)
		return
	}

	res := o.Handler.Handle(Params) // actually handle the request

	o.Context.Respond(rw, r, route.Produces, route, res)

}

// CardReaderConfigByHashBody card reader config by hash body
// swagger:model CardReaderConfigByHashBody
type CardReaderConfigByHashBody struct {

	// hash
	// Required: true
	Hash model.Hash `json:"hash"`
}

// Validate validates this card reader config by hash body
func (o *CardReaderConfigByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CardReaderConfigByHashBody) validateHash(formats strfmt.Registry) error {

	if err := o.Hash.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("args" + "." + "hash")
		}
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *CardReaderConfigByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CardReaderConfigByHashBody) UnmarshalBinary(b []byte) error {
	var res CardReaderConfigByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}