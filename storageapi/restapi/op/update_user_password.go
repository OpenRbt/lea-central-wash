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

// UpdateUserPasswordHandlerFunc turns a function with the right signature into a update user password handler
type UpdateUserPasswordHandlerFunc func(UpdateUserPasswordParams, *storageapi.Profile) UpdateUserPasswordResponder

// Handle executing the request and returning a response
func (fn UpdateUserPasswordHandlerFunc) Handle(params UpdateUserPasswordParams, principal *storageapi.Profile) UpdateUserPasswordResponder {
	return fn(params, principal)
}

// UpdateUserPasswordHandler interface for that can handle valid update user password params
type UpdateUserPasswordHandler interface {
	Handle(UpdateUserPasswordParams, *storageapi.Profile) UpdateUserPasswordResponder
}

// NewUpdateUserPassword creates a new http.Handler for the update user password operation
func NewUpdateUserPassword(ctx *middleware.Context, handler UpdateUserPasswordHandler) *UpdateUserPassword {
	return &UpdateUserPassword{Context: ctx, Handler: handler}
}

/* UpdateUserPassword swagger:route POST /user-password updateUserPassword

UpdateUserPassword update user password API

*/
type UpdateUserPassword struct {
	Context *middleware.Context
	Handler UpdateUserPasswordHandler
}

func (o *UpdateUserPassword) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewUpdateUserPasswordParams()
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

// UpdateUserPasswordBody ArgUserPassword
//
// swagger:model UpdateUserPasswordBody
type UpdateUserPasswordBody struct {

	// login
	// Required: true
	Login *model.Login `json:"login"`

	// new password
	// Required: true
	NewPassword *model.Password `json:"newPassword"`

	// old password
	// Required: true
	OldPassword *model.Password `json:"oldPassword"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *UpdateUserPasswordBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// login
		// Required: true
		Login *model.Login `json:"login"`

		// new password
		// Required: true
		NewPassword *model.Password `json:"newPassword"`

		// old password
		// Required: true
		OldPassword *model.Password `json:"oldPassword"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Login = props.Login
	o.NewPassword = props.NewPassword
	o.OldPassword = props.OldPassword
	return nil
}

// Validate validates this update user password body
func (o *UpdateUserPasswordBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateLogin(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateNewPassword(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateOldPassword(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserPasswordBody) validateLogin(formats strfmt.Registry) error {

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
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "login")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserPasswordBody) validateNewPassword(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"newPassword", "body", o.NewPassword); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"newPassword", "body", o.NewPassword); err != nil {
		return err
	}

	if o.NewPassword != nil {
		if err := o.NewPassword.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "newPassword")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "newPassword")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserPasswordBody) validateOldPassword(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"oldPassword", "body", o.OldPassword); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"oldPassword", "body", o.OldPassword); err != nil {
		return err
	}

	if o.OldPassword != nil {
		if err := o.OldPassword.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "oldPassword")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "oldPassword")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this update user password body based on the context it is used
func (o *UpdateUserPasswordBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateLogin(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateNewPassword(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateOldPassword(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserPasswordBody) contextValidateLogin(ctx context.Context, formats strfmt.Registry) error {

	if o.Login != nil {

		if err := o.Login.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "login")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "login")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserPasswordBody) contextValidateNewPassword(ctx context.Context, formats strfmt.Registry) error {

	if o.NewPassword != nil {

		if err := o.NewPassword.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "newPassword")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "newPassword")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserPasswordBody) contextValidateOldPassword(ctx context.Context, formats strfmt.Registry) error {

	if o.OldPassword != nil {

		if err := o.OldPassword.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "oldPassword")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "oldPassword")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *UpdateUserPasswordBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *UpdateUserPasswordBody) UnmarshalBinary(b []byte) error {
	var res UpdateUserPasswordBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// UpdateUserPasswordCreatedBody ResponseUserPassword
//
// swagger:model UpdateUserPasswordCreatedBody
type UpdateUserPasswordCreatedBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *UpdateUserPasswordCreatedBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this update user password created body
func (o *UpdateUserPasswordCreatedBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserPasswordCreatedBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("updateUserPasswordCreated"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this update user password created body based on context it is used
func (o *UpdateUserPasswordCreatedBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *UpdateUserPasswordCreatedBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *UpdateUserPasswordCreatedBody) UnmarshalBinary(b []byte) error {
	var res UpdateUserPasswordCreatedBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
