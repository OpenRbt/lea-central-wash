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

	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// DeleteUserHandlerFunc turns a function with the right signature into a delete user handler
type DeleteUserHandlerFunc func(DeleteUserParams, *storageapi.Profile) DeleteUserResponder

// Handle executing the request and returning a response
func (fn DeleteUserHandlerFunc) Handle(params DeleteUserParams, principal *storageapi.Profile) DeleteUserResponder {
	return fn(params, principal)
}

// DeleteUserHandler interface for that can handle valid delete user params
type DeleteUserHandler interface {
	Handle(DeleteUserParams, *storageapi.Profile) DeleteUserResponder
}

// NewDeleteUser creates a new http.Handler for the delete user operation
func NewDeleteUser(ctx *middleware.Context, handler DeleteUserHandler) *DeleteUser {
	return &DeleteUser{Context: ctx, Handler: handler}
}

/* DeleteUser swagger:route DELETE /user deleteUser

DeleteUser delete user API

*/
type DeleteUser struct {
	Context *middleware.Context
	Handler DeleteUserHandler
}

func (o *DeleteUser) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewDeleteUserParams()
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

// DeleteUserBody delete user body
//
// swagger:model DeleteUserBody
type DeleteUserBody struct {

	// login
	// Required: true
	Login *model.Login `json:"login"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DeleteUserBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// login
		// Required: true
		Login *model.Login `json:"login"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Login = props.Login
	return nil
}

// Validate validates this delete user body
func (o *DeleteUserBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateLogin(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DeleteUserBody) validateLogin(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"login", "body", o.Login); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"login", "body", o.Login); err != nil {
		return err
	}

	if o.Login != nil {
		if err := o.Login.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "login")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this delete user body based on the context it is used
func (o *DeleteUserBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateLogin(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DeleteUserBody) contextValidateLogin(ctx context.Context, formats strfmt.Registry) error {

	if o.Login != nil {
		if err := o.Login.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "login")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *DeleteUserBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DeleteUserBody) UnmarshalBinary(b []byte) error {
	var res DeleteUserBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// DeleteUserConflictBody delete user conflict body
//
// swagger:model DeleteUserConflictBody
type DeleteUserConflictBody struct {

	// code
	// Required: true
	Code *int64 `json:"code"`

	// message
	// Required: true
	Message *string `json:"message"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *DeleteUserConflictBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// code
		// Required: true
		Code *int64 `json:"code"`

		// message
		// Required: true
		Message *string `json:"message"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Code = props.Code
	o.Message = props.Message
	return nil
}

// Validate validates this delete user conflict body
func (o *DeleteUserConflictBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateCode(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateMessage(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *DeleteUserConflictBody) validateCode(formats strfmt.Registry) error {

	if err := validate.Required("deleteUserConflict"+"."+"code", "body", o.Code); err != nil {
		return err
	}

	return nil
}

func (o *DeleteUserConflictBody) validateMessage(formats strfmt.Registry) error {

	if err := validate.Required("deleteUserConflict"+"."+"message", "body", o.Message); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this delete user conflict body based on context it is used
func (o *DeleteUserConflictBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *DeleteUserConflictBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *DeleteUserConflictBody) UnmarshalBinary(b []byte) error {
	var res DeleteUserConflictBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
