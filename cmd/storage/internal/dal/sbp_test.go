package dal

import (
	"testing"

	"github.com/powerman/check"
)

func TestPaymentCase(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	// save
	err := testRepo.SavePayment(testPayment1)
	t.Nil(err)

	// get
	p, err := testRepo.GetPaymentByOrderID(testPayment1.OrderID)
	t.Nil(err)
	t.DeepEqual(p, testPayment1)

	// cancel case
	err = testRepo.SetPaymentCanceled(testPayment1.OrderID)
	t.Nil(err)

	pCancel, err := testRepo.GetPaymentByOrderID(testPayment1.OrderID)
	t.Nil(err)
	t.Equal(pCancel.Canceled, true)

	// authorize case
	err = testRepo.SetPaymentAuthorized(testPayment1.OrderID)
	t.Nil(err)
	pConfirmed, err := testRepo.GetPaymentByOrderID(testPayment1.OrderID)
	t.Nil(err)
	t.Equal(pConfirmed.Authorized, true)

	// received case
	err = testRepo.SetPaymentReceived(testPayment1.OrderID)
	t.Nil(err)
	pReceived, err := testRepo.GetPaymentByOrderID(testPayment1.OrderID)
	t.Nil(err)
	t.Equal(pReceived.OpenwashReceived, true)
}
