// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"

	"github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// SaveIfNotExistsReader is a Reader for the SaveIfNotExists structure.
type SaveIfNotExistsReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SaveIfNotExistsReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSaveIfNotExistsNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewSaveIfNotExistsNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSaveIfNotExistsInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("[POST /save-if-not-exists] saveIfNotExists", response, response.Code())
	}
}

// NewSaveIfNotExistsNoContent creates a SaveIfNotExistsNoContent with default headers values
func NewSaveIfNotExistsNoContent() *SaveIfNotExistsNoContent {
	return &SaveIfNotExistsNoContent{}
}

/*
SaveIfNotExistsNoContent describes a response with status code 204, with default header values.

OK
*/
type SaveIfNotExistsNoContent struct {
}

// IsSuccess returns true when this save if not exists no content response has a 2xx status code
func (o *SaveIfNotExistsNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this save if not exists no content response has a 3xx status code
func (o *SaveIfNotExistsNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save if not exists no content response has a 4xx status code
func (o *SaveIfNotExistsNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this save if not exists no content response has a 5xx status code
func (o *SaveIfNotExistsNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this save if not exists no content response a status code equal to that given
func (o *SaveIfNotExistsNoContent) IsCode(code int) bool {
	return code == 204
}

// Code gets the status code for the save if not exists no content response
func (o *SaveIfNotExistsNoContent) Code() int {
	return 204
}

func (o *SaveIfNotExistsNoContent) Error() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsNoContent ", 204)
}

func (o *SaveIfNotExistsNoContent) String() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsNoContent ", 204)
}

func (o *SaveIfNotExistsNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveIfNotExistsNotFound creates a SaveIfNotExistsNotFound with default headers values
func NewSaveIfNotExistsNotFound() *SaveIfNotExistsNotFound {
	return &SaveIfNotExistsNotFound{}
}

/*
SaveIfNotExistsNotFound describes a response with status code 404, with default header values.

not found
*/
type SaveIfNotExistsNotFound struct {
}

// IsSuccess returns true when this save if not exists not found response has a 2xx status code
func (o *SaveIfNotExistsNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save if not exists not found response has a 3xx status code
func (o *SaveIfNotExistsNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save if not exists not found response has a 4xx status code
func (o *SaveIfNotExistsNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this save if not exists not found response has a 5xx status code
func (o *SaveIfNotExistsNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this save if not exists not found response a status code equal to that given
func (o *SaveIfNotExistsNotFound) IsCode(code int) bool {
	return code == 404
}

// Code gets the status code for the save if not exists not found response
func (o *SaveIfNotExistsNotFound) Code() int {
	return 404
}

func (o *SaveIfNotExistsNotFound) Error() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsNotFound ", 404)
}

func (o *SaveIfNotExistsNotFound) String() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsNotFound ", 404)
}

func (o *SaveIfNotExistsNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveIfNotExistsInternalServerError creates a SaveIfNotExistsInternalServerError with default headers values
func NewSaveIfNotExistsInternalServerError() *SaveIfNotExistsInternalServerError {
	return &SaveIfNotExistsInternalServerError{}
}

/*
SaveIfNotExistsInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SaveIfNotExistsInternalServerError struct {
}

// IsSuccess returns true when this save if not exists internal server error response has a 2xx status code
func (o *SaveIfNotExistsInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save if not exists internal server error response has a 3xx status code
func (o *SaveIfNotExistsInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save if not exists internal server error response has a 4xx status code
func (o *SaveIfNotExistsInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this save if not exists internal server error response has a 5xx status code
func (o *SaveIfNotExistsInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this save if not exists internal server error response a status code equal to that given
func (o *SaveIfNotExistsInternalServerError) IsCode(code int) bool {
	return code == 500
}

// Code gets the status code for the save if not exists internal server error response
func (o *SaveIfNotExistsInternalServerError) Code() int {
	return 500
}

func (o *SaveIfNotExistsInternalServerError) Error() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsInternalServerError ", 500)
}

func (o *SaveIfNotExistsInternalServerError) String() string {
	return fmt.Sprintf("[POST /save-if-not-exists][%d] saveIfNotExistsInternalServerError ", 500)
}

func (o *SaveIfNotExistsInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*
SaveIfNotExistsBody ArgSaveIfNotExists
swagger:model SaveIfNotExistsBody
*/
type SaveIfNotExistsBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// key pair
	// Required: true
	KeyPair *model.KeyPair `json:"keyPair"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SaveIfNotExistsBody) UnmarshalJSON(data []byte) error {
	var props struct {

		// hash
		// Required: true
		Hash *model.Hash `json:"hash"`

		// key pair
		// Required: true
		KeyPair *model.KeyPair `json:"keyPair"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	o.Hash = props.Hash
	o.KeyPair = props.KeyPair
	return nil
}

// Validate validates this save if not exists body
func (o *SaveIfNotExistsBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if err := o.validateKeyPair(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SaveIfNotExistsBody) validateHash(formats strfmt.Registry) error {

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
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

func (o *SaveIfNotExistsBody) validateKeyPair(formats strfmt.Registry) error {

	if err := validate.Required("args"+"."+"keyPair", "body", o.KeyPair); err != nil {
		return err
	}

	if o.KeyPair != nil {
		if err := o.KeyPair.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "keyPair")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "keyPair")
			}
			return err
		}
	}

	return nil
}

// ContextValidate validate this save if not exists body based on the context it is used
func (o *SaveIfNotExistsBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := o.contextValidateHash(ctx, formats); err != nil {
		res = append(res, err)
	}

	if err := o.contextValidateKeyPair(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *SaveIfNotExistsBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

	if o.Hash != nil {

		if err := o.Hash.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "hash")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "hash")
			}
			return err
		}
	}

	return nil
}

func (o *SaveIfNotExistsBody) contextValidateKeyPair(ctx context.Context, formats strfmt.Registry) error {

	if o.KeyPair != nil {

		if err := o.KeyPair.ContextValidate(ctx, formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("args" + "." + "keyPair")
			} else if ce, ok := err.(*errors.CompositeError); ok {
				return ce.ValidateName("args" + "." + "keyPair")
			}
			return err
		}
	}

	return nil
}

// MarshalBinary interface implementation
func (o *SaveIfNotExistsBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SaveIfNotExistsBody) UnmarshalBinary(b []byte) error {
	var res SaveIfNotExistsBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
