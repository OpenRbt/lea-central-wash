// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"io"

	"github.com/go-openapi/runtime"

	strfmt "github.com/go-openapi/strfmt"

	model "github.com/DiaElectronics/lea-central-wash/storageapi/model"
)

// StationsKeyPairReader is a Reader for the StationsKeyPair structure.
type StationsKeyPairReader struct {
	formats strfmt.Registry
}

// ReadResponse reads a server response into the received o.
func (o *StationsKeyPairReader) ReadResponse(response runtime.ClientResponse, consumer runtime.Consumer) (interface{}, error) {
	switch response.Code() {

	case 200:
		result := NewStationsKeyPairOK()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return result, nil

	case 500:
		result := NewStationsKeyPairInternalServerError()
		if err := result.readResponse(response, consumer, o.formats); err != nil {
			return nil, err
		}
		return nil, result

	default:
		return nil, runtime.NewAPIError("unknown error", response, response.Code())
	}
}

// NewStationsKeyPairOK creates a StationsKeyPairOK with default headers values
func NewStationsKeyPairOK() *StationsKeyPairOK {
	return &StationsKeyPairOK{}
}

/*StationsKeyPairOK handles this case with default header values.

OK
*/
type StationsKeyPairOK struct {
	Payload []*model.StationKeyPair
}

func (o *StationsKeyPairOK) Error() string {
	return fmt.Sprintf("[POST /stations-key-pair][%d] stationsKeyPairOK  %+v", 200, o.Payload)
}

func (o *StationsKeyPairOK) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	// response payload
	if err := consumer.Consume(response.Body(), &o.Payload); err != nil && err != io.EOF {
		return err
	}

	return nil
}

// NewStationsKeyPairInternalServerError creates a StationsKeyPairInternalServerError with default headers values
func NewStationsKeyPairInternalServerError() *StationsKeyPairInternalServerError {
	return &StationsKeyPairInternalServerError{}
}

/*StationsKeyPairInternalServerError handles this case with default header values.

internal error
*/
type StationsKeyPairInternalServerError struct {
}

func (o *StationsKeyPairInternalServerError) Error() string {
	return fmt.Sprintf("[POST /stations-key-pair][%d] stationsKeyPairInternalServerError ", 500)
}

func (o *StationsKeyPairInternalServerError) readResponse(response runtime.ClientResponse, consumer runtime.Consumer, formats strfmt.Registry) error {

	return nil
}
