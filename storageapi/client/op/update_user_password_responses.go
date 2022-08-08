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

// UpdateUserPasswordReader is a Reader for the UpdateUserPassword structure.
type UpdateUserPasswordReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *UpdateUserPasswordReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 201:
		result := NewUpdateUserPasswordCreated()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 401:
		result := NewUpdateUserPasswordUnauthorized()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 403:
		result := NewUpdateUserPasswordForbidden()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 404:
		result := NewUpdateUserPasswordNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewUpdateUserPasswordInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewUpdateUserPasswordCreated creates a UpdateUserPasswordCreated with default headers values
func NewUpdateUserPasswordCreated() *UpdateUserPasswordCreated {
	return &UpdateUserPasswordCreated{}
}

/* UpdateUserPasswordCreated describes a response with status code 201, with default header values.

OK
*/
type UpdateUserPasswordCreated struct {
	Payload *UpdateUserPasswordCreatedBody
}

// IsSuccess returns true when this update user password created response has a 2xx status code
func (o *UpdateUserPasswordCreated) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this update user password created response has a 3xx status code
func (o *UpdateUserPasswordCreated) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user password created response has a 4xx status code
func (o *UpdateUserPasswordCreated) IsClientError() bool {
	return false
}

// IsServerError returns true when this update user password created response has a 5xx status code
func (o *UpdateUserPasswordCreated) IsServerError() bool {
	return false
}

// IsCode returns true when this update user password created response a status code equal to that given
func (o *UpdateUserPasswordCreated) IsCode(code int) bool {
	return code == 201
}

func (o *UpdateUserPasswordCreated) Error() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordCreated  %+v", 201, o.Payload)
}

func (o *UpdateUserPasswordCreated) String() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordCreated  %+v", 201, o.Payload)
}

func (o *UpdateUserPasswordCreated) GetPayload() *UpdateUserPasswordCreatedBody {
	return o.Payload
}

func (o *UpdateUserPasswordCreated) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(UpdateUserPasswordCreatedBody)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewUpdateUserPasswordUnauthorized creates a UpdateUserPasswordUnauthorized with default headers values
func NewUpdateUserPasswordUnauthorized() *UpdateUserPasswordUnauthorized {
	return &UpdateUserPasswordUnauthorized{}
}

/* UpdateUserPasswordUnauthorized describes a response with status code 401, with default header values.

PIN is missing or invalid
*/
type UpdateUserPasswordUnauthorized struct {
}

// IsSuccess returns true when this update user password unauthorized response has a 2xx status code
func (o *UpdateUserPasswordUnauthorized) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user password unauthorized response has a 3xx status code
func (o *UpdateUserPasswordUnauthorized) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user password unauthorized response has a 4xx status code
func (o *UpdateUserPasswordUnauthorized) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user password unauthorized response has a 5xx status code
func (o *UpdateUserPasswordUnauthorized) IsServerError() bool {
	return false
}

// IsCode returns true when this update user password unauthorized response a status code equal to that given
func (o *UpdateUserPasswordUnauthorized) IsCode(code int) bool {
	return code == 401
}

func (o *UpdateUserPasswordUnauthorized) Error() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordUnauthorized ", 401)
}

func (o *UpdateUserPasswordUnauthorized) String() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordUnauthorized ", 401)
}

func (o *UpdateUserPasswordUnauthorized) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserPasswordForbidden creates a UpdateUserPasswordForbidden with default headers values
func NewUpdateUserPasswordForbidden() *UpdateUserPasswordForbidden {
	return &UpdateUserPasswordForbidden{}
}

/* UpdateUserPasswordForbidden describes a response with status code 403, with default header values.

Access forbidden
*/
type UpdateUserPasswordForbidden struct {
}

// IsSuccess returns true when this update user password forbidden response has a 2xx status code
func (o *UpdateUserPasswordForbidden) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user password forbidden response has a 3xx status code
func (o *UpdateUserPasswordForbidden) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user password forbidden response has a 4xx status code
func (o *UpdateUserPasswordForbidden) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user password forbidden response has a 5xx status code
func (o *UpdateUserPasswordForbidden) IsServerError() bool {
	return false
}

// IsCode returns true when this update user password forbidden response a status code equal to that given
func (o *UpdateUserPasswordForbidden) IsCode(code int) bool {
	return code == 403
}

func (o *UpdateUserPasswordForbidden) Error() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordForbidden ", 403)
}

func (o *UpdateUserPasswordForbidden) String() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordForbidden ", 403)
}

func (o *UpdateUserPasswordForbidden) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserPasswordNotFound creates a UpdateUserPasswordNotFound with default headers values
func NewUpdateUserPasswordNotFound() *UpdateUserPasswordNotFound {
	return &UpdateUserPasswordNotFound{}
}

/* UpdateUserPasswordNotFound describes a response with status code 404, with default header values.

not found
*/
type UpdateUserPasswordNotFound struct {
}

// IsSuccess returns true when this update user password not found response has a 2xx status code
func (o *UpdateUserPasswordNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user password not found response has a 3xx status code
func (o *UpdateUserPasswordNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user password not found response has a 4xx status code
func (o *UpdateUserPasswordNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this update user password not found response has a 5xx status code
func (o *UpdateUserPasswordNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this update user password not found response a status code equal to that given
func (o *UpdateUserPasswordNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *UpdateUserPasswordNotFound) Error() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordNotFound ", 404)
}

func (o *UpdateUserPasswordNotFound) String() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordNotFound ", 404)
}

func (o *UpdateUserPasswordNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewUpdateUserPasswordInternalServerError creates a UpdateUserPasswordInternalServerError with default headers values
func NewUpdateUserPasswordInternalServerError() *UpdateUserPasswordInternalServerError {
	return &UpdateUserPasswordInternalServerError{}
}

/* UpdateUserPasswordInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type UpdateUserPasswordInternalServerError struct {
}

// IsSuccess returns true when this update user password internal server error response has a 2xx status code
func (o *UpdateUserPasswordInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this update user password internal server error response has a 3xx status code
func (o *UpdateUserPasswordInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this update user password internal server error response has a 4xx status code
func (o *UpdateUserPasswordInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this update user password internal server error response has a 5xx status code
func (o *UpdateUserPasswordInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this update user password internal server error response a status code equal to that given
func (o *UpdateUserPasswordInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *UpdateUserPasswordInternalServerError) Error() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordInternalServerError ", 500)
}

func (o *UpdateUserPasswordInternalServerError) String() string {
	return fmt.Sprintf("[POST /user-password][%d] updateUserPasswordInternalServerError ", 500)
}

func (o *UpdateUserPasswordInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*UpdateUserPasswordBody ArgUserPassword
swagger:model UpdateUserPasswordBody
*/
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

/*UpdateUserPasswordCreatedBody ResponseUserPassword
swagger:model UpdateUserPasswordCreatedBody
*/
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
