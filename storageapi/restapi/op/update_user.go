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
	validate "github.com/go-openapi/validate"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// UpdateUserHandlerFunc turns a function with the right signature into a update user handler
type UpdateUserHandlerFunc func(UpdateUserParams, *storageapi.Profile) UpdateUserResponder

// Handle executing the request and returning a response
func (fn UpdateUserHandlerFunc) Handle(params UpdateUserParams, principal *storageapi.Profile) UpdateUserResponder {
	return fn(params, principal)
}

// UpdateUserHandler interface for that can handle valid update user params
type UpdateUserHandler interface {
	Handle(UpdateUserParams, *storageapi.Profile) UpdateUserResponder
}

// NewUpdateUser creates a new http.Handler for the update user operation
func NewUpdateUser(ctx *middleware.Context, handler UpdateUserHandler) *UpdateUser {
	return &UpdateUser{Context: ctx, Handler: handler}
}

/*UpdateUser swagger:route PUT /user updateUser

UpdateUser update user API

*/
type UpdateUser struct {
	Context *middleware.Context
	Handler UpdateUserHandler
}

func (o *UpdateUser) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		r = rCtx
	}
	var Params = NewUpdateUserParams()

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

// UpdateUserBody update user body
// swagger:model UpdateUserBody
type UpdateUserBody struct {

	// first name
	FirstName *model.FirstName `json:"firstName,omitempty"`

	// is admin
	IsAdmin *model.IsAdmin `json:"isAdmin,omitempty"`

	// is engineer
	IsEngineer *model.IsEngineer `json:"isEngineer,omitempty"`

	// is operator
	IsOperator *model.IsOperator `json:"isOperator,omitempty"`

	// last name
	LastName *model.LastName `json:"lastName,omitempty"`

	// login
	// Required: true
	Login model.Login `json:"login"`

	// middle name
	MiddleName *model.MiddleName `json:"middleName,omitempty"`
}

// Validate validates this update user body
func (o *UpdateUserBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateFirstName(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateLastName(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateLogin(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateMiddleName(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserBody) validateFirstName(formats strfmt.Registry) error {

	if swag.IsZero(o.FirstName) { // not required
		return nil
	}

	if o.FirstName != nil {
		if err := o.FirstName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "firstName")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserBody) validateLastName(formats strfmt.Registry) error {

	if swag.IsZero(o.LastName) { // not required
		return nil
	}

	if o.LastName != nil {
		if err := o.LastName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "lastName")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserBody) validateLogin(formats strfmt.Registry) error {

	if err := o.Login.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("args" + "." + "login")
		}
		return err
	}

	return nil
}

func (o *UpdateUserBody) validateMiddleName(formats strfmt.Registry) error {

	if swag.IsZero(o.MiddleName) { // not required
		return nil
	}

	if o.MiddleName != nil {
		if err := o.MiddleName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "middleName")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *UpdateUserBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *UpdateUserBody) UnmarshalBinary(b []byte) error {
	var res UpdateUserBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// UpdateUserCreatedBody update user created body
// swagger:model UpdateUserCreatedBody
type UpdateUserCreatedBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// Validate validates this update user created body
func (o *UpdateUserCreatedBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserCreatedBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("updateUserCreated"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *UpdateUserCreatedBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *UpdateUserCreatedBody) UnmarshalBinary(b []byte) error {
	var res UpdateUserCreatedBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
