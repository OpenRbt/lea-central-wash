package app

// FindLastReport is to find the latest report saved for a station
func (a *app) FindLastReport(StationID int) (*MoneyReport, error) {
	lastCachedReport, ok := a.lastReports[StationID]
	if ok {
		return &lastCachedReport, nil
	}
	lastDBReport, err := a.repo.LastMoneyReport(StationID)
	if err == ErrNotFound {
		return &MoneyReport{}, nil
	}
	if err != nil {
		return nil, err
	}
	return &lastDBReport, nil
}

// FindReportDifference gets calculated difference. If current report is less that what saved in the database
// we must calculate the difference and always add it,
func (a *app) FindReportDifference(StationID int) *MoneyReport {
	resultDifference, ok := a.reportsDifferences[StationID]
	if ok {
		return &resultDifference
	}
	return &MoneyReport{}
}

// UpdatedReport calculates the report we must save in the database, returns updatedReport and new reportDifference
func (a *app) UpdatedReport(ReceivedReport, LastCachedReport, ReportDifference *MoneyReport) (FinalReport, NewReportDiff MoneyReport) {
	FinalReport = *ReceivedReport
	NewReportDiff = *ReportDifference

	if !ReportDifference.IsZero() {
		FinalReport.AddUp(ReportDifference)
	}
	additionalDifference := FinalReport.AlignWith(LastCachedReport)
	if !additionalDifference.IsZero() {
		FinalReport.AddUp(&additionalDifference)
		NewReportDiff.AddUp(&additionalDifference)
	}
	return FinalReport, NewReportDiff
}

// SaveReportDifference just saves the calculated difference in cache
func (a *app) SaveReportDifference(StationID int, ReportDifference *MoneyReport) {
	a.reportsDifferences[StationID] = *ReportDifference
}

// SaveLastReportJust updates
func (a *app) SaveLastReport(StationID int, UpdatedReport *MoneyReport) {
	a.lastReports[StationID] = *UpdatedReport
}
