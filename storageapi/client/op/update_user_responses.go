// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// UpdateUserReader is a Reader for the UpdateUser structure.
type UpdateUserReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *UpdateUserReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 201:
		result := NewUpdateUserCreated()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewUpdateUserUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewUpdateUserForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 404:
		result := NewUpdateUserNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewUpdateUserInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[PUT /user] updateUser", response, response.Code())
	}
}

// NewUpdateUserCreated creates a UpdateUserCreated with default headers values
func NewUpdateUserCreated() *UpdateUserCreated {
	return &UpdateUserCreated{}
}

/*
UpdateUserCreated describes a response with status code 201, with default header values.

OK
*/
type UpdateUserCreated struct {
	Payload *UpdateUserCreatedBody
}

// IsSuccess returns true when this update user created response has a 2xx status code
func (o *UpdateUserCreated) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this update user created response has a 3xx status code
func (o *UpdateUserCreated) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user created response has a 4xx status code
func (o *UpdateUserCreated) IsClientError() bool {
	return false
}

// IsServerError returns true when this update user created response has a 5xx status code
func (o *UpdateUserCreated) IsServerError() bool {
	return false
}

// IsCode returns true when this update user created response a status code equal to that given
func (o *UpdateUserCreated) IsCode(code int) bool {
	return code == 201
}

// Code gets the status code for the update user created response
func (o *UpdateUserCreated) Code() int {
	return 201
}

func (o *UpdateUserCreated) Error() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserCreated  %+v", 201, o.Payload)
}

func (o *UpdateUserCreated) String() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserCreated  %+v", 201, o.Payload)
}

func (o *UpdateUserCreated) GetPayload() *UpdateUserCreatedBody {
	return o.Payload
}

func (o *UpdateUserCreated) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(UpdateUserCreatedBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewUpdateUserUnauthorized creates a UpdateUserUnauthorized with default headers values
func NewUpdateUserUnauthorized() *UpdateUserUnauthorized {
	return &UpdateUserUnauthorized{}
}

/*
UpdateUserUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type UpdateUserUnauthorized struct {
}

// IsSuccess returns true when this update user unauthorized response has a 2xx status code
func (o *UpdateUserUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user unauthorized response has a 3xx status code
func (o *UpdateUserUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user unauthorized response has a 4xx status code
func (o *UpdateUserUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user unauthorized response has a 5xx status code
func (o *UpdateUserUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this update user unauthorized response a status code equal to that given
func (o *UpdateUserUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the update user unauthorized response
func (o *UpdateUserUnauthorized) Code() int {
	return 401
}

func (o *UpdateUserUnauthorized) Error() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserUnauthorized ", 401)
}

func (o *UpdateUserUnauthorized) String() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserUnauthorized ", 401)
}

func (o *UpdateUserUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserForbidden creates a UpdateUserForbidden with default headers values
func NewUpdateUserForbidden() *UpdateUserForbidden {
	return &UpdateUserForbidden{}
}

/*
UpdateUserForbidden describes a response with status code 403, with default header values.

Access forbidden
*/
type UpdateUserForbidden struct {
}

// IsSuccess returns true when this update user forbidden response has a 2xx status code
func (o *UpdateUserForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user forbidden response has a 3xx status code
func (o *UpdateUserForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user forbidden response has a 4xx status code
func (o *UpdateUserForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user forbidden response has a 5xx status code
func (o *UpdateUserForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this update user forbidden response a status code equal to that given
func (o *UpdateUserForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the update user forbidden response
func (o *UpdateUserForbidden) Code() int {
	return 403
}

func (o *UpdateUserForbidden) Error() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserForbidden ", 403)
}

func (o *UpdateUserForbidden) String() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserForbidden ", 403)
}

func (o *UpdateUserForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserNotFound creates a UpdateUserNotFound with default headers values
func NewUpdateUserNotFound() *UpdateUserNotFound {
	return &UpdateUserNotFound{}
}

/*
UpdateUserNotFound describes a response with status code 404, with default header values.

Not found
*/
type UpdateUserNotFound struct {
}

// IsSuccess returns true when this update user not found response has a 2xx status code
func (o *UpdateUserNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user not found response has a 3xx status code
func (o *UpdateUserNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user not found response has a 4xx status code
func (o *UpdateUserNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user not found response has a 5xx status code
func (o *UpdateUserNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this update user not found response a status code equal to that given
func (o *UpdateUserNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the update user not found response
func (o *UpdateUserNotFound) Code() int {
	return 404
}

func (o *UpdateUserNotFound) Error() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserNotFound ", 404)
}

func (o *UpdateUserNotFound) String() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserNotFound ", 404)
}

func (o *UpdateUserNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserInternalServerError creates a UpdateUserInternalServerError with default headers values
func NewUpdateUserInternalServerError() *UpdateUserInternalServerError {
	return &UpdateUserInternalServerError{}
}

/*
UpdateUserInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type UpdateUserInternalServerError struct {
}

// IsSuccess returns true when this update user internal server error response has a 2xx status code
func (o *UpdateUserInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user internal server error response has a 3xx status code
func (o *UpdateUserInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user internal server error response has a 4xx status code
func (o *UpdateUserInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this update user internal server error response has a 5xx status code
func (o *UpdateUserInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this update user internal server error response a status code equal to that given
func (o *UpdateUserInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the update user internal server error response
func (o *UpdateUserInternalServerError) Code() int {
	return 500
}

func (o *UpdateUserInternalServerError) Error() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserInternalServerError ", 500)
}

func (o *UpdateUserInternalServerError) String() string {
	return fmt.Sprintf("[PUT /user][%d] updateUserInternalServerError ", 500)
}

func (o *UpdateUserInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
UpdateUserBody ArgUserUpdate
swagger:model UpdateUserBody
*/
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
	Login *model.Login `json:"login"`

	// middle name
	MiddleName *model.MiddleName `json:"middleName,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *UpdateUserBody) UnmarshalJSON(data []byte) error {
	var props struct {

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
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.FirstName = props.FirstName
	o.IsAdmin = props.IsAdmin
	o.IsEngineer = props.IsEngineer
	o.IsOperator = props.IsOperator
	o.LastName = props.LastName
	o.Login = props.Login
	o.MiddleName = props.MiddleName
	return nil
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
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "firstName")
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
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "lastName")
			}
			return err
		}
	}

	return nil
}

func (o *UpdateUserBody) validateLogin(formats strfmt.Registry) error {

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

func (o *UpdateUserBody) validateMiddleName(formats strfmt.Registry) error {
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

// ContextValidate validate this update user body based on the context it is used
func (o *UpdateUserBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
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

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *UpdateUserBody) contextValidateFirstName(ctx context.Context, formats strfmt.Registry) error {

	if o.FirstName != nil {

		if swag.IsZero(o.FirstName) { // not required
			return nil
		}

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

func (o *UpdateUserBody) contextValidateIsAdmin(ctx context.Context, formats strfmt.Registry) error {

	if o.IsAdmin != nil {

		if swag.IsZero(o.IsAdmin) { // not required
			return nil
		}

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

func (o *UpdateUserBody) contextValidateIsEngineer(ctx context.Context, formats strfmt.Registry) error {

	if o.IsEngineer != nil {

		if swag.IsZero(o.IsEngineer) { // not required
			return nil
		}

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

func (o *UpdateUserBody) contextValidateIsOperator(ctx context.Context, formats strfmt.Registry) error {

	if o.IsOperator != nil {

		if swag.IsZero(o.IsOperator) { // not required
			return nil
		}

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

func (o *UpdateUserBody) contextValidateLastName(ctx context.Context, formats strfmt.Registry) error {

	if o.LastName != nil {

		if swag.IsZero(o.LastName) { // not required
			return nil
		}

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

func (o *UpdateUserBody) contextValidateLogin(ctx context.Context, formats strfmt.Registry) error {

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

func (o *UpdateUserBody) contextValidateMiddleName(ctx context.Context, formats strfmt.Registry) error {

	if o.MiddleName != nil {

		if swag.IsZero(o.MiddleName) { // not required
			return nil
		}

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

/*
UpdateUserCreatedBody ResponseUserUpdate
swagger:model UpdateUserCreatedBody
*/
type UpdateUserCreatedBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *UpdateUserCreatedBody) UnmarshalJSON(data []byte) error {
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

// ContextValidate validates this update user created body based on context it is used
func (o *UpdateUserCreatedBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
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
