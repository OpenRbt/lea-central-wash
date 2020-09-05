package app_test

import (
	"fmt"
	"testing"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/memdb"
	"github.com/google/go-cmp/cmp"
)

var (
	FirstStationID = 1
	FirstHash      = "B1B2B3B4B5B6"
)

func NewTestApplication(repo app.Repo) app.App {
	if repo == nil {
		repo = memdb.New()
	}
	application := app.New(repo, nil)
	station := app.SetStationParams{
		Hash: FirstHash,
		ID:   FirstStationID,
		Name: "unicode ЯRNИ",
	}
	application.SetStation(station)

	return application
}

func SampleMoneyReport() app.MoneyReport {
	report := app.MoneyReport{}
	report.Hash = FirstHash
	report.Banknotes = 1000
	report.CarsTotal = 2
	report.Coins = 50
	report.Electronical = 25120
	report.Service = 700
	report.StationID = 1
	return report
}

func SampleNegativeReport() app.MoneyReport {
	return app.MoneyReport{
		Banknotes:    -1000,
		CarsTotal:    -2,
		Coins:        -50,
		Electronical: -25120,
		Service:      -700,
	}
}

func SmallNegativePiece() app.MoneyReport {
	return app.MoneyReport{
		Banknotes:    -1,
		Coins:        -1,
		Electronical: -1,
		Service:      -1,
		CarsTotal:    -1,
	}
}

func SampleDiff() app.MoneyReport {
	return app.MoneyReport{
		Banknotes:    100,
		CarsTotal:    1,
		Coins:        10,
		Electronical: 180,
		Service:      100,
	}
}

func TestSmokeApp(t *testing.T) {
	application := NewTestApplication(nil)
	foundID, err := application.FindStationIDByHash(FirstHash)
	if err != nil {
		t.Error(err)
	}
	if foundID != FirstStationID {
		t.Errorf("crap ID found: expected %+v, found: %+v\n", FirstStationID, foundID)
	}
}
func TestSaveRecoverReport(t *testing.T) {
	application := NewTestApplication(nil)
	reportPtr, err := application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	if !reportPtr.IsZero() {
		t.Error("expected zero report")
	}

	report := SampleMoneyReport()
	application.SaveLastReport(FirstStationID, &report)

	receivedReport, err := application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	if cmp.Equal(*reportPtr, *receivedReport) {
		t.Error("reports must not be equal on step 1")
	}
	if !cmp.Equal(report, *receivedReport) {
		t.Error("reports are not equal on step 1")
	}

	// let's update and save again
	report.Banknotes = 300
	report.CarsTotal = 11
	report.Electronical = 23210
	report.Coins = 22
	report.Service = 3300

	application.SaveLastReport(FirstStationID, &report)
	receivedReport, err = application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	if !cmp.Equal(report, *receivedReport) {
		t.Error("updated report is not equal")
	}
}

func TestAddUp(t *testing.T) {
	originalReport := SampleMoneyReport()
	report := SampleMoneyReport()
	diff := SampleDiff()
	report.AddUp(&diff)
	correctReport := app.MoneyReport{
		Banknotes:    originalReport.Banknotes + diff.Banknotes,
		Coins:        originalReport.Coins + diff.Coins,
		Electronical: originalReport.Electronical + diff.Electronical,
		CarsTotal:    originalReport.CarsTotal + diff.CarsTotal,
		Service:      originalReport.Service + diff.Service,
	}
	if !cmp.Equal(report, correctReport) {
		t.Error()
	}
}

func TestNegativeDifferenceCalcInternalFuncs(t *testing.T) {
	application := NewTestApplication(nil)
	negativeReport := SampleNegativeReport()

	application.SaveLastReport(FirstStationID, &negativeReport)
	lastReport, err := application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	diff := application.FindReportDifference(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	finalRep, newDiff := application.UpdatedReport(&negativeReport, lastReport, diff)
	if !finalRep.IsZero() {
		t.Error("difference has not bring the report to zero")
	}
	application.SaveLastReport(FirstStationID, &finalRep)
	application.SaveReportDifference(FirstStationID, &newDiff)

	lastRep2, err := application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	lastDiff2 := application.FindReportDifference(FirstStationID)
	if err != nil {
		t.Error(err)
	}

	addedMoney := SampleDiff()
	negativeReport.AddUp(&addedMoney)

	finalRep, newDiff = application.UpdatedReport(&negativeReport, lastRep2, lastDiff2)

	if !cmp.Equal(finalRep, addedMoney) {
		t.Error("wrong report calculated")
	}

	application.SaveLastReport(FirstStationID, &finalRep)
	application.SaveReportDifference(FirstStationID, &newDiff)

	lastRep3, err := application.FindLastReport(FirstStationID)
	if err != nil {
		t.Error(err)
	}
	lastDiff3 := application.FindReportDifference(FirstStationID)
	if err != nil {
		t.Error(err)
	}

	smallNegative := SmallNegativePiece()
	negativeReport.AddUp(&smallNegative)
	finalRepToCheck, newDiff := application.UpdatedReport(&negativeReport, lastRep3, lastDiff3)
	if !cmp.Equal(finalRep, finalRepToCheck) {
		t.Error("report should not have changed")
	}

	newDiff.AddUp(&smallNegative)

	if !cmp.Equal(newDiff, *lastDiff3) {
		t.Error("wrong difference for broken positive report")
	}
}

func TestNegativeDifferenceCalcAppLayer(t *testing.T) {
	application := NewTestApplication(nil)

	report := SampleMoneyReport()
	report.StationID = FirstStationID
	fmt.Printf("trying to save the report")
	application.SaveMoneyReport(report)
	loadedRep, err := application.LoadMoneyReport(report.Hash)
	if !cmp.Equal(report, *loadedRep) {
		t.Error("save-load reports must be equal")
		return
	}

	negativePiece := SmallNegativePiece()
	report.AddUp(&negativePiece)
	application.SaveMoneyReport(report)
	originalReport := SampleMoneyReport()
	loadedRep, err = application.LoadMoneyReport(report.Hash)
	if err != nil {
		t.Error(err)
	}
	fmt.Printf("=====\n%+v\n%+v\n%+v\n", report, originalReport, *loadedRep)
	if !cmp.Equal(originalReport, *loadedRep) {
		t.Error("reports must be equal")
	}
	application.SaveMoneyReport(originalReport)
	loadedRep, err = application.LoadMoneyReport(report.Hash)
	loadedRep.AddUp(&negativePiece)
	if !cmp.Equal(originalReport, *loadedRep) {
		t.Error("reports must be equal again")
	}
}

func TestReportIsPushedToDB(t *testing.T) {
	DB := memdb.New()
	application := NewTestApplication(DB)
	report := SampleMoneyReport()
	err := application.SaveMoneyReport(report)
	if err != nil {
		t.Error(err)
	}
	anotherApplication := NewTestApplication(DB)
	loadedReport, err := anotherApplication.LoadMoneyReport(FirstHash)
	if !cmp.Equal(report, *loadedReport) {
		t.Error("Can't load the report :(")
	}
}
