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

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// CreateUserReader is a Reader for the CreateUser structure.
type CreateUserReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *CreateUserReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 201:
		result := NewCreateUserCreated()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewCreateUserUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewCreateUserForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 409:
		result := NewCreateUserConflict()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewCreateUserInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewCreateUserCreated creates a CreateUserCreated with default headers values
func NewCreateUserCreated() *CreateUserCreated {
	return &CreateUserCreated{}
}

/*
CreateUserCreated describes a response with status code 201, with default header values.

OK
*/
type CreateUserCreated struct {
	Payload *CreateUserCreatedBody
}

// IsSuccess returns true when this create user created response has a 2xx status code
func (o *CreateUserCreated) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this create user created response has a 3xx status code
func (o *CreateUserCreated) IsRedirect() bool {
	return false
}

// IsClientError returns true when this create user created response has a 4xx status code
func (o *CreateUserCreated) IsClientError() bool {
	return false
}

// IsServerError returns true when this create user created response has a 5xx status code
func (o *CreateUserCreated) IsServerError() bool {
	return false
}

// IsCode returns true when this create user created response a status code equal to that given
func (o *CreateUserCreated) IsCode(code int) bool {
	return code == 201
}

// Code gets the status code for the create user created response
func (o *CreateUserCreated) Code() int {
	return 201
}

func (o *CreateUserCreated) Error() string {
	return fmt.Sprintf("[POST /user][%d] createUserCreated  %+v", 201, o.Payload)
}

func (o *CreateUserCreated) String() string {
	return fmt.Sprintf("[POST /user][%d] createUserCreated  %+v", 201, o.Payload)
}

func (o *CreateUserCreated) GetPayload() *CreateUserCreatedBody {
	return o.Payload
}

func (o *CreateUserCreated) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(CreateUserCreatedBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewCreateUserUnauthorized creates a CreateUserUnauthorized with default headers values
func NewCreateUserUnauthorized() *CreateUserUnauthorized {
	return &CreateUserUnauthorized{}
}

/*
CreateUserUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type CreateUserUnauthorized struct {
}

// IsSuccess returns true when this create user unauthorized response has a 2xx status code
func (o *CreateUserUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this create user unauthorized response has a 3xx status code
func (o *CreateUserUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this create user unauthorized response has a 4xx status code
func (o *CreateUserUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this create user unauthorized response has a 5xx status code
func (o *CreateUserUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this create user unauthorized response a status code equal to that given
func (o *CreateUserUnauthorized) IsCode(code int) bool {
	return code == 401
}

// Code gets the status code for the create user unauthorized response
func (o *CreateUserUnauthorized) Code() int {
	return 401
}

func (o *CreateUserUnauthorized) Error() string {
	return fmt.Sprintf("[POST /user][%d] createUserUnauthorized ", 401)
}

func (o *CreateUserUnauthorized) String() string {
	return fmt.Sprintf("[POST /user][%d] createUserUnauthorized ", 401)
}

func (o *CreateUserUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewCreateUserForbidden creates a CreateUserForbidden with default headers values
func NewCreateUserForbidden() *CreateUserForbidden {
	return &CreateUserForbidden{}
}

/*
CreateUserForbidden describes a response with status code 403, with default header values.

Access forbidden
*/
type CreateUserForbidden struct {
}

// IsSuccess returns true when this create user forbidden response has a 2xx status code
func (o *CreateUserForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this create user forbidden response has a 3xx status code
func (o *CreateUserForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this create user forbidden response has a 4xx status code
func (o *CreateUserForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this create user forbidden response has a 5xx status code
func (o *CreateUserForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this create user forbidden response a status code equal to that given
func (o *CreateUserForbidden) IsCode(code int) bool {
	return code == 403
}

// Code gets the status code for the create user forbidden response
func (o *CreateUserForbidden) Code() int {
	return 403
}

func (o *CreateUserForbidden) Error() string {
	return fmt.Sprintf("[POST /user][%d] createUserForbidden ", 403)
}

func (o *CreateUserForbidden) String() string {
	return fmt.Sprintf("[POST /user][%d] createUserForbidden ", 403)
}

func (o *CreateUserForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewCreateUserConflict creates a CreateUserConflict with default headers values
func NewCreateUserConflict() *CreateUserConflict {
	return &CreateUserConflict{}
}

/*
CreateUserConflict describes a response with status code 409, with default header values.

Conflict
*/
type CreateUserConflict struct {
	Payload *CreateUserConflictBody
}

// IsSuccess returns true when this create user conflict response has a 2xx status code
func (o *CreateUserConflict) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this create user conflict response has a 3xx status code
func (o *CreateUserConflict) IsRedirect() bool {
	return false
}

// IsClientError returns true when this create user conflict response has a 4xx status code
func (o *CreateUserConflict) IsClientError() bool {
	return true
}

// IsServerError returns true when this create user conflict response has a 5xx status code
func (o *CreateUserConflict) IsServerError() bool {
	return false
}

// IsCode returns true when this create user conflict response a status code equal to that given
func (o *CreateUserConflict) IsCode(code int) bool {
	return code == 409
}

// Code gets the status code for the create user conflict response
func (o *CreateUserConflict) Code() int {
	return 409
}

func (o *CreateUserConflict) Error() string {
	return fmt.Sprintf("[POST /user][%d] createUserConflict  %+v", 409, o.Payload)
}

func (o *CreateUserConflict) String() string {
	return fmt.Sprintf("[POST /user][%d] createUserConflict  %+v", 409, o.Payload)
}

func (o *CreateUserConflict) GetPayload() *CreateUserConflictBody {
	return o.Payload
}

func (o *CreateUserConflict) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(CreateUserConflictBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewCreateUserInternalServerError creates a CreateUserInternalServerError with default headers values
func NewCreateUserInternalServerError() *CreateUserInternalServerError {
	return &CreateUserInternalServerError{}
}

/*
CreateUserInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type CreateUserInternalServerError struct {
}

// IsSuccess returns true when this create user internal server error response has a 2xx status code
func (o *CreateUserInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this create user internal server error response has a 3xx status code
func (o *CreateUserInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this create user internal server error response has a 4xx status code
func (o *CreateUserInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this create user internal server error response has a 5xx status code
func (o *CreateUserInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this create user internal server error response a status code equal to that given
func (o *CreateUserInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the create user internal server error response
func (o *CreateUserInternalServerError) Code() int {
	return 500
}

func (o *CreateUserInternalServerError) Error() string {
	return fmt.Sprintf("[POST /user][%d] createUserInternalServerError ", 500)
}

func (o *CreateUserInternalServerError) String() string {
	return fmt.Sprintf("[POST /user][%d] createUserInternalServerError ", 500)
}

func (o *CreateUserInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
CreateUserBody ArgUserCreate
swagger:model CreateUserBody
*/
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

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CreateUserBody) UnmarshalJSON(data []byte) error {
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

		// password
		// Required: true
		Password *model.Password `json:"password"`
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
	o.Password = props.Password
	return nil
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

/*
CreateUserConflictBody ResponseUserCreateConflict
swagger:model CreateUserConflictBody
*/
type CreateUserConflictBody struct {

	// code
	// Required: true
	Code *int64 `json:"code"`

	// message
	// Required: true
	Message *string `json:"message"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CreateUserConflictBody) UnmarshalJSON(data []byte) error {
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

/*
CreateUserCreatedBody ResponseUserCreate
swagger:model CreateUserCreatedBody
*/
type CreateUserCreatedBody struct {

	// id
	// Required: true
	ID *int64 `json:"id"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *CreateUserCreatedBody) UnmarshalJSON(data []byte) error {
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
