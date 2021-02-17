// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/swag"

	strfmt "github.com/go-openapi/strfmt"

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationProgramByHashReader is a Reader for the StationProgramByHash structure.
type StationProgramByHashReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationProgramByHashReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 200:
		result := NewStationProgramByHashOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 500:
		result := NewStationProgramByHashInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewStationProgramByHashOK creates a StationProgramByHashOK with default headers values
func NewStationProgramByHashOK() *StationProgramByHashOK {
	return &StationProgramByHashOK{}
}

/*StationProgramByHashOK handles this case with default header values.

OK
*/
type StationProgramByHashOK struct {
	Payload *model.StationPrograms
}

func (o *StationProgramByHashOK) Error() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashOK  %+v", 200, o.Payload)
}

func (o *StationProgramByHashOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	o.Payload = new(model.StationPrograms)

	// response payload
	if err := consumer.Consume(response.Body(), o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationProgramByHashInternalServerError creates a StationProgramByHashInternalServerError with default headers values
func NewStationProgramByHashInternalServerError() *StationProgramByHashInternalServerError {
	return &StationProgramByHashInternalServerError{}
}

/*StationProgramByHashInternalServerError handles this case with default header values.

internal error
*/
type StationProgramByHashInternalServerError struct {
}

func (o *StationProgramByHashInternalServerError) Error() string {
	return fmt.Sprintf("[POST /station-program-by-hash][%d] stationProgramByHashInternalServerError ", 500)
}

func (o *StationProgramByHashInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}

/*StationProgramByHashBody station program by hash body
swagger:model StationProgramByHashBody
*/
type StationProgramByHashBody struct {

	// hash
	// Required: true
	Hash model.Hash `json:"hash"`
}

// Validate validates this station program by hash body
func (o *StationProgramByHashBody) Validate(formats strfmt.Registry) error {
	var res []error

	if err := o.validateHash(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (o *StationProgramByHashBody) validateHash(formats strfmt.Registry) error {

	if err := o.Hash.Validate(formats); err != nil {
		if ve, ok := err.(*errors.Validation); ok {
			return ve.ValidateName("args" + "." + "hash")
		}
		return err
	}

	return nil
}

// MarshalBinary interface implementation
func (o *StationProgramByHashBody) MarshalBinary() ([]byte, error) {
	if o == nil {
		return nil, nil
	}
	return swag.WriteJSON(o)
}

// UnmarshalBinary interface implementation
func (o *StationProgramByHashBody) UnmarshalBinary(b []byte) error {
	var res StationProgramByHashBody
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*o = res
	return nil
}
