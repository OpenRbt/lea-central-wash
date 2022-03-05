// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"
	"strconv"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// AdvertisingCampaign advertising campaign
//
// swagger:model AdvertisingCampaign
type AdvertisingCampaign struct {

	// default discount
	DefaultDiscount int64 `json:"defaultDiscount,omitempty"`

	// discount programs
	DiscountPrograms []*DiscountProgram `json:"discountPrograms"`

	// enabled
	Enabled bool `json:"enabled,omitempty"`

	// Unix time local
	// Required: true
	EndDate *int64 `json:"endDate"`

	// end minute
	// Maximum: 1440
	// Minimum: 0
	EndMinute *int64 `json:"endMinute,omitempty"`

	// id
	ID int64 `json:"id,omitempty"`

	// name
	Name string `json:"name,omitempty"`

	// Unix time local
	// Required: true
	StartDate *int64 `json:"startDate"`

	// start minute
	// Maximum: 1440
	// Minimum: 0
	StartMinute *int64 `json:"startMinute,omitempty"`

	// weekday
	Weekday []string `json:"weekday"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *AdvertisingCampaign) UnmarshalJSON(data []byte) error {
	var props struct {

		// default discount
		DefaultDiscount int64 `json:"defaultDiscount,omitempty"`

		// discount programs
		DiscountPrograms []*DiscountProgram `json:"discountPrograms"`

		// enabled
		Enabled bool `json:"enabled,omitempty"`

		// Unix time local
		// Required: true
		EndDate *int64 `json:"endDate"`

		// end minute
		// Maximum: 1440
		// Minimum: 0
		EndMinute *int64 `json:"endMinute,omitempty"`

		// id
		ID int64 `json:"id,omitempty"`

		// name
		Name string `json:"name,omitempty"`

		// Unix time local
		// Required: true
		StartDate *int64 `json:"startDate"`

		// start minute
		// Maximum: 1440
		// Minimum: 0
		StartMinute *int64 `json:"startMinute,omitempty"`

		// weekday
		Weekday []string `json:"weekday"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.DefaultDiscount = props.DefaultDiscount
	m.DiscountPrograms = props.DiscountPrograms
	m.Enabled = props.Enabled
	m.EndDate = props.EndDate
	m.EndMinute = props.EndMinute
	m.ID = props.ID
	m.Name = props.Name
	m.StartDate = props.StartDate
	m.StartMinute = props.StartMinute
	m.Weekday = props.Weekday
	return nil
}

// Validate validates this advertising campaign
func (m *AdvertisingCampaign) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateDiscountPrograms(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateEndDate(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateEndMinute(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateStartDate(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateStartMinute(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateWeekday(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *AdvertisingCampaign) validateDiscountPrograms(formats strfmt.Registry) error {
	if swag.IsZero(m.DiscountPrograms) { // not required
		return nil
	}

	for i := 0; i < len(m.DiscountPrograms); i++ {
		if swag.IsZero(m.DiscountPrograms[i]) { // not required
			continue
		}

		if m.DiscountPrograms[i] != nil {
			if err := m.DiscountPrograms[i].Validate(formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("discountPrograms" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

func (m *AdvertisingCampaign) validateEndDate(formats strfmt.Registry) error {

	if err := validate.Required("endDate", "body", m.EndDate); err != nil {
		return err
	}

	return nil
}

func (m *AdvertisingCampaign) validateEndMinute(formats strfmt.Registry) error {
	if swag.IsZero(m.EndMinute) { // not required
		return nil
	}

	if err := validate.MinimumInt("endMinute", "body", *m.EndMinute, 0, false); err != nil {
		return err
	}

	if err := validate.MaximumInt("endMinute", "body", *m.EndMinute, 1440, false); err != nil {
		return err
	}

	return nil
}

func (m *AdvertisingCampaign) validateStartDate(formats strfmt.Registry) error {

	if err := validate.Required("startDate", "body", m.StartDate); err != nil {
		return err
	}

	return nil
}

func (m *AdvertisingCampaign) validateStartMinute(formats strfmt.Registry) error {
	if swag.IsZero(m.StartMinute) { // not required
		return nil
	}

	if err := validate.MinimumInt("startMinute", "body", *m.StartMinute, 0, false); err != nil {
		return err
	}

	if err := validate.MaximumInt("startMinute", "body", *m.StartMinute, 1440, false); err != nil {
		return err
	}

	return nil
}

var advertisingCampaignWeekdayItemsEnum []interface{}

func init() {
	var res []string
	if err := json.Unmarshal([]byte(`["sunday","monday","tuesday","wednesday","thursday","friday","saturday"]`), &res); err != nil {
		panic(err)
	}
	for _, v := range res {
		advertisingCampaignWeekdayItemsEnum = append(advertisingCampaignWeekdayItemsEnum, v)
	}
}

func (m *AdvertisingCampaign) validateWeekdayItemsEnum(path, location string, value string) error {
	if err := validate.EnumCase(path, location, value, advertisingCampaignWeekdayItemsEnum, true); err != nil {
		return err
	}
	return nil
}

func (m *AdvertisingCampaign) validateWeekday(formats strfmt.Registry) error {
	if swag.IsZero(m.Weekday) { // not required
		return nil
	}

	for i := 0; i < len(m.Weekday); i++ {

		// value enum
		if err := m.validateWeekdayItemsEnum("weekday"+"."+strconv.Itoa(i), "body", m.Weekday[i]); err != nil {
			return err
		}

	}

	return nil
}

// ContextValidate validate this advertising campaign based on the context it is used
func (m *AdvertisingCampaign) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	var res []error

	if err := m.contextValidateDiscountPrograms(ctx, formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *AdvertisingCampaign) contextValidateDiscountPrograms(ctx context.Context, formats strfmt.Registry) error {

	for i := 0; i < len(m.DiscountPrograms); i++ {

		if m.DiscountPrograms[i] != nil {
			if err := m.DiscountPrograms[i].ContextValidate(ctx, formats); err != nil {
				if ve, ok := err.(*errors.Validation); ok {
					return ve.ValidateName("discountPrograms" + "." + strconv.Itoa(i))
				}
				return err
			}
		}

	}

	return nil
}

// MarshalBinary interface implementation
func (m *AdvertisingCampaign) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *AdvertisingCampaign) UnmarshalBinary(b []byte) error {
	var res AdvertisingCampaign
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
