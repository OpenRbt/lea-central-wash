// Code generated by go-swagger; DO NOT EDIT.

package model

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"bytes"
	"context"
	"encoding/json"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
	"github.com/go-openapi/validate"
)

// KasseConfig kasse config
//
// swagger:model KasseConfig
type KasseConfig struct {

	// cashier
	// Min Length: 1
	Cashier string `json:"cashier,omitempty"`

	// cashier i n n
	// Max Length: 12
	// Min Length: 12
	// Pattern: ^[0123456789]{12}$
	CashierINN string `json:"cashierINN,omitempty"`

	// receipt item name
	// Min Length: 1
	ReceiptItemName string `json:"receiptItemName,omitempty"`

	// tax
	// Enum: ["TAX_VAT110","TAX_VAT0","TAX_NO","TAX_VAT120"]
	Tax string `json:"tax,omitempty"`
}

// UnmarshalJSON unmarshals this object while disallowing additional properties from JSON
func (m *KasseConfig) UnmarshalJSON(data []byte) error {
	var props struct {

		// cashier
		// Min Length: 1
		Cashier string `json:"cashier,omitempty"`

		// cashier i n n
		// Max Length: 12
		// Min Length: 12
		// Pattern: ^[0123456789]{12}$
		CashierINN string `json:"cashierINN,omitempty"`

		// receipt item name
		// Min Length: 1
		ReceiptItemName string `json:"receiptItemName,omitempty"`

		// tax
		// Enum: ["TAX_VAT110","TAX_VAT0","TAX_NO","TAX_VAT120"]
		Tax string `json:"tax,omitempty"`
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&props); err != nil {
		return err
	}

	m.Cashier = props.Cashier
	m.CashierINN = props.CashierINN
	m.ReceiptItemName = props.ReceiptItemName
	m.Tax = props.Tax
	return nil
}

// Validate validates this kasse config
func (m *KasseConfig) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateCashier(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateCashierINN(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateReceiptItemName(formats); err != nil {
		res = append(res, err)
	}

	if err := m.validateTax(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *KasseConfig) validateCashier(formats strfmt.Registry) error {
	if swag.IsZero(m.Cashier) { // not required
		return nil
	}

	if err := validate.MinLength("cashier", "body", m.Cashier, 1); err != nil {
		return err
	}

	return nil
}

func (m *KasseConfig) validateCashierINN(formats strfmt.Registry) error {
	if swag.IsZero(m.CashierINN) { // not required
		return nil
	}

	if err := validate.MinLength("cashierINN", "body", m.CashierINN, 12); err != nil {
		return err
	}

	if err := validate.MaxLength("cashierINN", "body", m.CashierINN, 12); err != nil {
		return err
	}

	if err := validate.Pattern("cashierINN", "body", m.CashierINN, `^[0123456789]{12}$`); err != nil {
		return err
	}

	return nil
}

func (m *KasseConfig) validateReceiptItemName(formats strfmt.Registry) error {
	if swag.IsZero(m.ReceiptItemName) { // not required
		return nil
	}

	if err := validate.MinLength("receiptItemName", "body", m.ReceiptItemName, 1); err != nil {
		return err
	}

	return nil
}

var kasseConfigTypeTaxPropEnum []interface{}

func init() {
	var res []string
	if err := json.Unmarshal([]byte(`["TAX_VAT110","TAX_VAT0","TAX_NO","TAX_VAT120"]`), &res); err != nil {
		panic(err)
	}
	for _, v := range res {
		kasseConfigTypeTaxPropEnum = append(kasseConfigTypeTaxPropEnum, v)
	}
}

const (

	// KasseConfigTaxTAXVAT110 captures enum value "TAX_VAT110"
	KasseConfigTaxTAXVAT110 string = "TAX_VAT110"

	// KasseConfigTaxTAXVAT0 captures enum value "TAX_VAT0"
	KasseConfigTaxTAXVAT0 string = "TAX_VAT0"

	// KasseConfigTaxTAXNO captures enum value "TAX_NO"
	KasseConfigTaxTAXNO string = "TAX_NO"

	// KasseConfigTaxTAXVAT120 captures enum value "TAX_VAT120"
	KasseConfigTaxTAXVAT120 string = "TAX_VAT120"
)

// prop value enum
func (m *KasseConfig) validateTaxEnum(path, location string, value string) error {
	if err := validate.EnumCase(path, location, value, kasseConfigTypeTaxPropEnum, true); err != nil {
		return err
	}
	return nil
}

func (m *KasseConfig) validateTax(formats strfmt.Registry) error {
	if swag.IsZero(m.Tax) { // not required
		return nil
	}

	// value enum
	if err := m.validateTaxEnum("tax", "body", m.Tax); err != nil {
		return err
	}

	return nil
}

// ContextValidate validates this kasse config based on context it is used
func (m *KasseConfig) ContextValidate(ctx context.Context, formats strfmt.Registry) error {
	return nil
}

// MarshalBinary interface implementation
func (m *KasseConfig) MarshalBinary() ([]byte, error) {
	if m == nil {
		return nil, nil
	}
	return swag.WriteJSON(m)
}

// UnmarshalBinary interface implementation
func (m *KasseConfig) UnmarshalBinary(b []byte) error {
	var res KasseConfig
	if err := swag.ReadJSON(b, &res); err != nil {
		return err
	}
	*m = res
	return nil
}
