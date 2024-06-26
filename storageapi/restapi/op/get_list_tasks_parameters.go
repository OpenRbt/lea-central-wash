// Code generated by go-swagger; DO NOT EDIT.

package op

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"fmt"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// NewGetListTasksParams creates a new GetListTasksParams object
// with the default values initialized.
func NewGetListTasksParams() GetListTasksParams {

	var (
		// initialize parameters with default values

		pageDefault     = int64(1)
		pageSizeDefault = int64(10)
		sortDefault     = string("createdAtDesc")
	)

	return GetListTasksParams{
		Page: &pageDefault,

		PageSize: &pageSizeDefault,

		Sort: &sortDefault,
	}
}

// GetListTasksParams contains all the bound params for the get list tasks operation
// typically these are obtained from a http.Request
//
// swagger:parameters getListTasks
type GetListTasksParams struct {

	// HTTP Request Object
	HTTPRequest *http.Request `json:"-"`

	/*
	  Minimum: 1
	  In: query
	  Default: 1
	*/
	Page *int64
	/*
	  Maximum: 100
	  Minimum: 1
	  In: query
	  Default: 10
	*/
	PageSize *int64
	/*
	  In: query
	  Default: "createdAtDesc"
	*/
	Sort *string
	/*
	  In: query
	  Collection Format: multi
	*/
	StationsID []int64
	/*
	  In: query
	  Collection Format: multi
	*/
	Statuses []string
	/*
	  In: query
	  Collection Format: multi
	*/
	Types []string
}

// BindRequest both binds and validates a request, it assumes that complex things implement a Validatable(strfmt.Registry) error interface
// for simple values it will use straight method calls.
//
// To ensure default values, the struct must have been initialized with NewGetListTasksParams() beforehand.
func (o *GetListTasksParams) BindRequest(r *http.Request, route *middleware.MatchedRoute) error {
	var res []error

	o.HTTPRequest = r

	qs := runtime.Values(r.URL.Query())

	qPage, qhkPage, _ := qs.GetOK("page")
	if err := o.bindPage(qPage, qhkPage, route.Formats); err != nil {
		res = append(res, err)
	}

	qPageSize, qhkPageSize, _ := qs.GetOK("pageSize")
	if err := o.bindPageSize(qPageSize, qhkPageSize, route.Formats); err != nil {
		res = append(res, err)
	}

	qSort, qhkSort, _ := qs.GetOK("sort")
	if err := o.bindSort(qSort, qhkSort, route.Formats); err != nil {
		res = append(res, err)
	}

	qStationsID, qhkStationsID, _ := qs.GetOK("stationsID")
	if err := o.bindStationsID(qStationsID, qhkStationsID, route.Formats); err != nil {
		res = append(res, err)
	}

	qStatuses, qhkStatuses, _ := qs.GetOK("statuses")
	if err := o.bindStatuses(qStatuses, qhkStatuses, route.Formats); err != nil {
		res = append(res, err)
	}

	qTypes, qhkTypes, _ := qs.GetOK("types")
	if err := o.bindTypes(qTypes, qhkTypes, route.Formats); err != nil {
		res = append(res, err)
	}
	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

// bindPage binds and validates parameter Page from query.
func (o *GetListTasksParams) bindPage(rawData []string, hasKey bool, formats strfmt.Registry) error {
	var raw string
	if len(rawData) > 0 {
		raw = rawData[len(rawData)-1]
	}

	// Required: false
	// AllowEmptyValue: false

	if raw == "" { // empty values pass all other validations
		// Default values have been previously initialized by NewGetListTasksParams()
		return nil
	}

	value, err := swag.ConvertInt64(raw)
	if err != nil {
		return errors.InvalidType("page", "query", "int64", raw)
	}
	o.Page = &value

	if err := o.validatePage(formats); err != nil {
		return err
	}

	return nil
}

// validatePage carries on validations for parameter Page
func (o *GetListTasksParams) validatePage(formats strfmt.Registry) error {

	if err := validate.MinimumInt("page", "query", *o.Page, 1, false); err != nil {
		return err
	}

	return nil
}

// bindPageSize binds and validates parameter PageSize from query.
func (o *GetListTasksParams) bindPageSize(rawData []string, hasKey bool, formats strfmt.Registry) error {
	var raw string
	if len(rawData) > 0 {
		raw = rawData[len(rawData)-1]
	}

	// Required: false
	// AllowEmptyValue: false

	if raw == "" { // empty values pass all other validations
		// Default values have been previously initialized by NewGetListTasksParams()
		return nil
	}

	value, err := swag.ConvertInt64(raw)
	if err != nil {
		return errors.InvalidType("pageSize", "query", "int64", raw)
	}
	o.PageSize = &value

	if err := o.validatePageSize(formats); err != nil {
		return err
	}

	return nil
}

// validatePageSize carries on validations for parameter PageSize
func (o *GetListTasksParams) validatePageSize(formats strfmt.Registry) error {

	if err := validate.MinimumInt("pageSize", "query", *o.PageSize, 1, false); err != nil {
		return err
	}

	if err := validate.MaximumInt("pageSize", "query", *o.PageSize, 100, false); err != nil {
		return err
	}

	return nil
}

// bindSort binds and validates parameter Sort from query.
func (o *GetListTasksParams) bindSort(rawData []string, hasKey bool, formats strfmt.Registry) error {
	var raw string
	if len(rawData) > 0 {
		raw = rawData[len(rawData)-1]
	}

	// Required: false
	// AllowEmptyValue: false

	if raw == "" { // empty values pass all other validations
		// Default values have been previously initialized by NewGetListTasksParams()
		return nil
	}
	o.Sort = &raw

	if err := o.validateSort(formats); err != nil {
		return err
	}

	return nil
}

// validateSort carries on validations for parameter Sort
func (o *GetListTasksParams) validateSort(formats strfmt.Registry) error {

	if err := validate.EnumCase("sort", "query", *o.Sort, []interface{}{"createdAtAsc", "createdAtDesc"}, true); err != nil {
		return err
	}

	return nil
}

// bindStationsID binds and validates array parameter StationsID from query.
//
// Arrays are parsed according to CollectionFormat: "multi" (defaults to "csv" when empty).
func (o *GetListTasksParams) bindStationsID(rawData []string, hasKey bool, formats strfmt.Registry) error {
	// CollectionFormat: multi
	stationsIDIC := rawData
	if len(stationsIDIC) == 0 {
		return nil
	}

	var stationsIDIR []int64
	for i, stationsIDIV := range stationsIDIC {
		stationsIDI, err := swag.ConvertInt64(stationsIDIV)
		if err != nil {
			return errors.InvalidType(fmt.Sprintf("%s.%v", "stationsID", i), "query", "int64", stationsIDI)
		}

		stationsIDIR = append(stationsIDIR, stationsIDI)
	}

	o.StationsID = stationsIDIR

	return nil
}

// bindStatuses binds and validates array parameter Statuses from query.
//
// Arrays are parsed according to CollectionFormat: "multi" (defaults to "csv" when empty).
func (o *GetListTasksParams) bindStatuses(rawData []string, hasKey bool, formats strfmt.Registry) error {
	// CollectionFormat: multi
	statusesIC := rawData
	if len(statusesIC) == 0 {
		return nil
	}

	var statusesIR []string
	for i, statusesIV := range statusesIC {
		statusesI := statusesIV

		if err := validate.EnumCase(fmt.Sprintf("%s.%v", "statuses", i), "query", statusesI, []interface{}{"queue", "started", "completed", "error", "canceled"}, true); err != nil {
			return err
		}

		statusesIR = append(statusesIR, statusesI)
	}

	o.Statuses = statusesIR

	return nil
}

// bindTypes binds and validates array parameter Types from query.
//
// Arrays are parsed according to CollectionFormat: "multi" (defaults to "csv" when empty).
func (o *GetListTasksParams) bindTypes(rawData []string, hasKey bool, formats strfmt.Registry) error {
	// CollectionFormat: multi
	typesIC := rawData
	if len(typesIC) == 0 {
		return nil
	}

	var typesIR []string
	for i, typesIV := range typesIC {
		typesI := typesIV

		if err := validate.EnumCase(fmt.Sprintf("%s.%v", "types", i), "query", typesI, []interface{}{"build", "update", "reboot", "getVersions", "pullFirmware", "setVersion"}, true); err != nil {
			return err
		}

		typesIR = append(typesIR, typesI)
	}

	o.Types = typesIR

	return nil
}
