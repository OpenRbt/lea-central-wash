package app

// IsZero checks if report money is zero
func (m *MoneyReport) IsZero() bool {
	if m == nil {
		return true
	}
	// real money is empty
	if m.Banknotes != 0 || m.Electronical != 0 || m.Coins != 0 {
		return false
	}
	// other stuff is empty
	if m.CarsTotal != 0 || m.Service != 0 {
		return false
	}
	return true
}

// AddUp adds up another values
func (m *MoneyReport) AddUp(AnotherMoneyReport *MoneyReport) {
	if m == nil || AnotherMoneyReport == nil {
		return
	}
	m.Banknotes += AnotherMoneyReport.Banknotes
	m.Coins += AnotherMoneyReport.Coins
	m.Electronical += AnotherMoneyReport.Electronical
	m.CarsTotal += AnotherMoneyReport.CarsTotal
	m.Service += AnotherMoneyReport.Service
}

// AlignWith returns a difference which allows adding up current report to not-negative values
func (m *MoneyReport) AlignWith(AnotherMoneyReport *MoneyReport) MoneyReport {
	difference := MoneyReport{}
	if m == nil || AnotherMoneyReport == nil {
		return difference
	}
	difference.Banknotes = howMuchToZero(m.Banknotes, AnotherMoneyReport.Banknotes)
	difference.Coins = howMuchToZero(m.Coins, AnotherMoneyReport.Coins)
	difference.Electronical = howMuchToZero(m.Electronical, AnotherMoneyReport.Electronical)
	difference.CarsTotal = howMuchToZero(m.CarsTotal, AnotherMoneyReport.CarsTotal)
	difference.Service = howMuchToZero(m.Service, AnotherMoneyReport.Service)
	return difference
}

func howMuchToZero(Source, AlignWith int) int {
	if AlignWith < 0 {
		AlignWith = 0
	}
	if AlignWith > Source {
		return AlignWith - Source
	}
	return 0
}
