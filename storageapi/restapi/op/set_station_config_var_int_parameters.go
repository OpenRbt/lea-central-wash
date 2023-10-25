// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"io"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/validate"

	"github.com/OpenRbt/lea-central-wash/storageapi/model"
)

// NewSetStationConfigVarIntParams creates a new SetStationConfigVarIntParams object
//
// There are no default values defined in the spec.
func NewSetStationConfigVarIntParams() SetStationConfigVarIntParams {

	return SetStationConfigVarIntParams{}
}

// SetStationConfigVarIntParams contains all the bound params for the set station config var int operation
// typically these are obtained from a http.Request
//
// swagger:parameters setStationConfigVarInt
type SetStationConfigVarIntParams struct {

	// HTTP Request Object
	HTTPRequest *http.Request `json:"-"`

	/*
	  Required: true
	  In: body
	*/
	Args *model.StationConfigVarInt
}

// BindRequest both binds and validates a request, it assumes that complex things implement a Validatable(strfmt.Registry) error interface
// for simple values it will use straight method calls.
//
// To ensure default values, the struct must have been initialized with NewSetStationConfigVarIntParams() beforehand.
func (o *SetStationConfigVarIntParams) BindRequest(r *http.Request, route *middleware.MatchedRoute) error {
	var res []error

	o.HTTPRequest = r

	if runtime.HasBody(r) {
		defer r.Body.Close()
		var body model.StationConfigVarInt
		if err := route.Consumer.Consume(r.Body, &body); err != nil {
			if err == io.EOF {
				res = append(res, errors.Required("args", "body", ""))
			} else {
				res = append(res, errors.NewParseError("args", "body", "", err))
			}
		} else {
			// validate body object
			if err := body.Validate(route.Formats); err != nil {
				res = append(res, err)
			}

			ctx := validate.WithOperationRequest(r.Context())
			if err := body.ContextValidate(ctx, route.Formats); err != nil {
				res = append(res, err)
			}

			if len(res) == 0 {
				o.Args = &body
			}
		}
	} else {
		res = append(res, errors.Required("args", "body", ""))
	}
	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}
