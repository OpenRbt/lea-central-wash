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

	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// CreateUserHandlerFunc turns a function with the right signature into a create user handler
type CreateUserHandlerFunc func(CreateUserParams, *storageapi.Profile) CreateUserResponder

// Handle executing the request and returning a response
func (fn CreateUserHandlerFunc) Handle(params CreateUserParams, principal *storageapi.Profile) CreateUserResponder {
	return fn(params, principal)
}

// CreateUserHandler interface for that can handle valid create user params
type CreateUserHandler interface {
	Handle(CreateUserParams, *storageapi.Profile) CreateUserResponder
}

// NewCreateUser creates a new http.Handler for the create user operation
func NewCreateUser(ctx *middleware.Context, handler CreateUserHandler) *CreateUser {
	return &CreateUser{Context: ctx, Handler: handler}
}

/*
	CreateUser swagger:route POST /user createUser

CreateUser create user API
*/
type CreateUser struct {
	Context *middleware.Context
	Handler CreateUserHandler
}

func (o *CreateUser) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	route, rCtx, _ := o.Context.RouteInfo(r)
	if rCtx != nil {
		*r = *rCtx
	}
	var Params = NewCreateUserParams()
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

// CreateUserBody ArgUserCreate
//
// swagger:model CreateUserBody
type CreateUserBody struct {

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
	Login *model.Login `json:"login"`

	// middle name
	MiddleName *model.MiddleName `json:"middleName,omitempty"`

	// password
	// Required: true
	Password *model.Password `json:"password"`
}

// Validate validates this create user body
func (o *CreateUserBody) Validate(formats strfmt.Registry) error {
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

	if err := o.validatePassword(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CreateUserBody) validateFirstName(formats strfmt.Registry) error {
	if swag.IsZero(o.FirstName) { // not required
		return nil
	}

	if o.FirstName != nil {
		if err := o.FirstName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "firstName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "firstName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) validateLastName(formats strfmt.Registry) error {
	if swag.IsZero(o.LastName) { // not required
		return nil
	}

	if o.LastName != nil {
		if err := o.LastName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "lastName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "lastName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) validateLogin(formats strfmt.Registry) error {

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

func (o *CreateUserBody) validateMiddleName(formats strfmt.Registry) error {
	if swag.IsZero(o.MiddleName) { // not required
		return nil
	}

	if o.MiddleName != nil {
		if err := o.MiddleName.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "middleName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "middleName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) validatePassword(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"password", "body", o.Password); err != nil {
		return err
	}

	if err := validate.Required("args"+"."+"password", "body", o.Password); err != nil {
		return err
	}

	if o.Password != nil {
		if err := o.Password.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "password")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "password")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this create user body based on the context it is used
func (o *CreateUserBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateFirstName(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateIsAdmin(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateIsEngineer(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateIsOperator(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateLastName(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateLogin(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateMiddleName(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidatePassword(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CreateUserBody) contextValidateFirstName(ctx context.Context, formats strfmt.Registry) error {

	if o.FirstName != nil {
		if err := o.FirstName.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "firstName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "firstName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidateIsAdmin(ctx context.Context, formats strfmt.Registry) error {

	if o.IsAdmin != nil {
		if err := o.IsAdmin.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "isAdmin")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "isAdmin")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidateIsEngineer(ctx context.Context, formats strfmt.Registry) error {

	if o.IsEngineer != nil {
		if err := o.IsEngineer.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "isEngineer")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "isEngineer")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidateIsOperator(ctx context.Context, formats strfmt.Registry) error {

	if o.IsOperator != nil {
		if err := o.IsOperator.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "isOperator")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "isOperator")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidateLastName(ctx context.Context, formats strfmt.Registry) error {

	if o.LastName != nil {
		if err := o.LastName.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "lastName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "lastName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidateLogin(ctx context.Context, formats strfmt.Registry) error {

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

func (o *CreateUserBody) contextValidateMiddleName(ctx context.Context, formats strfmt.Registry) error {

	if o.MiddleName != nil {
		if err := o.MiddleName.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "middleName")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "middleName")
			}
			return err
		}
	}

	return nil
}

func (o *CreateUserBody) contextValidatePassword(ctx context.Context, formats strfmt.Registry) error {

	if o.Password != nil {
		if err := o.Password.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "password")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "password")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *CreateUserBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CreateUserBody) UnmarshalBinary(b []byte) error {
	var res CreateUserBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// CreateUserConflictBody ResponseUserCreateConflict
//
// swagger:model CreateUserConflictBody
type CreateUserConflictBody struct {

	// code
	// Required: true
	Code *int64 `json:"code"`

	// message
	// Required: true
	Message *string `json:"message"`
}

// Validate validates this create user conflict body
func (o *CreateUserConflictBody) Validate(formats strfmt.Registry) error {
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

func (o *CreateUserConflictBody) validateCode(formats strfmt.Registry) error {

	if err := validate.Required("createUserConflict"+"."+"code", "body", o.Code); err != nil {
		return err
	}

	return nil
}

func (o *CreateUserConflictBody) validateMessage(formats strfmt.Registry) error {

	if err := validate.Required("createUserConflict"+"."+"message", "body", o.Message); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this create user conflict body based on context it is used
func (o *CreateUserConflictBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *CreateUserConflictBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CreateUserConflictBody) UnmarshalBinary(b []byte) error {
	var res CreateUserConflictBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}

// CreateUserCreatedBody ResponseUserCreate
//
// swagger:model CreateUserCreatedBody
type CreateUserCreatedBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// Validate validates this create user created body
func (o *CreateUserCreatedBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateID(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *CreateUserCreatedBody) validateID(formats strfmt.Registry) error {

	if err := validate.Required("createUserCreated"+"."+"id", "body", o.ID); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this create user created body based on context it is used
func (o *CreateUserCreatedBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (o *CreateUserCreatedBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *CreateUserCreatedBody) UnmarshalBinary(b []byte) error {
	var res CreateUserCreatedBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
