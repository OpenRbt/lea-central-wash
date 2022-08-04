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

// SaveReader is a Reader for the Save structure.
type SaveReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *SaveReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {
	case 204:
		result := NewSaveNoContent()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil
	case 404:
		result := NewSaveNotFound()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	case 500:
		result := NewSaveInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result
	default:
		return nil, runtime.NewAPIError("response status code does not match any response statuses defined for this endpoint in the swagger spec", response, response.Code())
	}
}

// NewSaveNoContent creates a SaveNoContent with default headers values
func NewSaveNoContent() *SaveNoContent {
	return &SaveNoContent{}
}

/* SaveNoContent describes a response with status code 204, with default header values.

OK
*/
type SaveNoContent struct {
}

// IsSuccess returns true when this save no content response has a 2xx status code
func (o *SaveNoContent) IsSuccess() bool {
	return true
}

// IsRedirect returns true when this save no content response has a 3xx status code
func (o *SaveNoContent) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save no content response has a 4xx status code
func (o *SaveNoContent) IsClientError() bool {
	return false
}

// IsServerError returns true when this save no content response has a 5xx status code
func (o *SaveNoContent) IsServerError() bool {
	return false
}

// IsCode returns true when this save no content response a status code equal to that given
func (o *SaveNoContent) IsCode(code int) bool {
	return code == 204
}

func (o *SaveNoContent) Error() string {
	return fmt.Sprintf("[POST /save][%d] saveNoContent ", 204)
}

func (o *SaveNoContent) String() string {
	return fmt.Sprintf("[POST /save][%d] saveNoContent ", 204)
}

func (o *SaveNoContent) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveNotFound creates a SaveNotFound with default headers values
func NewSaveNotFound() *SaveNotFound {
	return &SaveNotFound{}
}

/* SaveNotFound describes a response with status code 404, with default header values.

not found
*/
type SaveNotFound struct {
}

// IsSuccess returns true when this save not found response has a 2xx status code
func (o *SaveNotFound) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save not found response has a 3xx status code
func (o *SaveNotFound) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save not found response has a 4xx status code
func (o *SaveNotFound) IsClientError() bool {
	return true
}

// IsServerError returns true when this save not found response has a 5xx status code
func (o *SaveNotFound) IsServerError() bool {
	return false
}

// IsCode returns true when this save not found response a status code equal to that given
func (o *SaveNotFound) IsCode(code int) bool {
	return code == 404
}

func (o *SaveNotFound) Error() string {
	return fmt.Sprintf("[POST /save][%d] saveNotFound ", 404)
}

func (o *SaveNotFound) String() string {
	return fmt.Sprintf("[POST /save][%d] saveNotFound ", 404)
}

func (o *SaveNotFound) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

// NewSaveInternalServerError creates a SaveInternalServerError with default headers values
func NewSaveInternalServerError() *SaveInternalServerError {
	return &SaveInternalServerError{}
}

/* SaveInternalServerError describes a response with status code 500, with default header values.

internal error
*/
type SaveInternalServerError struct {
}

// IsSuccess returns true when this save internal server error response has a 2xx status code
func (o *SaveInternalServerError) IsSuccess() bool {
	return false
}

// IsRedirect returns true when this save internal server error response has a 3xx status code
func (o *SaveInternalServerError) IsRedirect() bool {
	return false
}

// IsClientError returns true when this save internal server error response has a 4xx status code
func (o *SaveInternalServerError) IsClientError() bool {
	return false
}

// IsServerError returns true when this save internal server error response has a 5xx status code
func (o *SaveInternalServerError) IsServerError() bool {
	return true
}

// IsCode returns true when this save internal server error response a status code equal to that given
func (o *SaveInternalServerError) IsCode(code int) bool {
	return code == 500
}

func (o *SaveInternalServerError) Error() string {
	return fmt.Sprintf("[POST /save][%d] saveInternalServerError ", 500)
}

func (o *SaveInternalServerError) String() string {
	return fmt.Sprintf("[POST /save][%d] saveInternalServerError ", 500)
}

func (o *SaveInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*SaveBody ArgSave
swagger:model SaveBody
*/
type SaveBody struct {

	// hash
	// Required: true
	Hash *model.Hash `json:"hash"`

	// key pair
	// Required: true
	KeyPair *model.KeyPair `json:"keyPair"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (o *SaveBody) UnmarshalJSON(data []byte) error {
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

// Validate validates this save body
func (o *SaveBody) Validate(formats strfmt.Registry) error {
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

func (o *SaveBody) validateHash(formats strfmt.Registry) error {

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

func (o *SaveBody) validateKeyPair(formats strfmt.Registry) error {

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

// ContextValidate validate this save body based on the context it is used
func (o *SaveBody) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
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

func (o *SaveBody) contextValidateHash(ctx context.Context, formats strfmt.Registry) error {

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

func (o *SaveBody) contextValidateKeyPair(ctx context.Context, formats strfmt.Registry) error {

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
func (o *SaveBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *SaveBody) UnmarshalBinary(b []byte) error {
	var res SaveBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
