package sbpclient

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	rabbit_vo "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/sbp-client/entity/vo"

	amqp "github.com/rabbitmq/amqp091-go"
)

type addMoneyReport struct {
	WashServerID       string
	MessageID          string
	CollectionReportID string
	IsLastCollection   bool
	StationID          int
	Banknotes          int
	CarsTotal          int
	Coins              int
	Electronical       int
	Service            int
	Bonuses            int
	QrMoney            int
	Ctime              time.Time
}

type addCollectionReport struct {
	WashServerID string
	ID           string
	StationID    int
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
	QrMoney      int
	Ctime        time.Time
}

const (
	addMoneyReportMsg      = "management_client_service/add_money_report"
	addCollectionReportMsg = "management_client_service/add_collection_report"
	washStatus             = "management_client_service/wash_status"
)

// Status describes station or kasse status.
type Status string

// Status.
const (
	StatusOffline Status = "offline"
	StatusOnline  Status = "online"
)

// StatusReport is just a status information
type StatusReport struct {
	WashServerID string
	KasseInfo    string
	KasseStatus  Status
	LCWInfo      string
	Stations     []StationStatus
	BonusStatus  ServiceStatus
	SbpStatus    ServiceStatus
	MngtStatus   ServiceStatus
}

// StationStatus is used to display in the managment software
type StationStatus struct {
	ID             int
	Info           string
	Name           string
	Status         Status
	CurrentBalance int
	CurrentProgram int
	ProgramName    string
	IP             string
}

type ServiceStatus struct {
	Available        bool
	DisabledOnServer bool
	IsConnected      bool
	LastErr          string
	DateLastErr      *time.Time
	UnpaidStations   map[int]bool
	ReconnectCount   int64
}

// ProcessSbpMessage ...
func (s *Service) ProcessSbpMessage(d amqp.Delivery) error {
	// debug
	fmt.Println(d.Type)
	fmt.Println(string(d.Body))
	switch d.Type {

	default:
		{
			err := d.Nack(false, false)
			if err != nil {
				return err
			}
			return nil
		}
	}

	//return nil
}

func (s *Service) SendMoneyReport(report app.MngtMoneyReport) (err error) {
	err = s.sendMessage(addMoneyReport{
		WashServerID:       s.serverID,
		MessageID:          report.ManagementMessageID.String(),
		CollectionReportID: report.CollectionReportID.String(),
		IsLastCollection:   report.IsLastCollection(),
		StationID:          int(report.StationID),
		Banknotes:          report.Banknotes,
		CarsTotal:          report.CarsTotal,
		Coins:              report.Coins,
		Electronical:       report.Electronical,
		Service:            report.Service,
		Bonuses:            report.Bonuses,
		QrMoney:            report.QrMoney,
		Ctime:              report.Ctime,
	}, addMoneyReportMsg)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendCollectionReport(report app.CollectionReport) (err error) {
	err = s.sendMessage(addCollectionReport{
		WashServerID: s.serverID,
		ID:           report.ManagementID.String(),
		StationID:    int(report.StationID),
		Banknotes:    report.Banknotes,
		CarsTotal:    report.CarsTotal,
		Coins:        report.Coins,
		Electronical: report.Electronical,
		Service:      report.Service,
		Bonuses:      report.Bonuses,
		QrMoney:      report.QrMoney,
		Ctime:        report.Ctime,
	}, addCollectionReportMsg)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) SendStatus(report app.StatusReport) (err error) {
	err = s.sendMessage(s.msgStatusReport(report), washStatus)
	if err != nil {
		s.setLastErr(err.Error())
	}
	return err
}

func (s *Service) sendMessage(msg interface{}, messageType rabbit_vo.MessageType) (err error) {
	jsonMsg, ok := msg.([]byte)
	if !ok {
		jsonMsg, err = json.Marshal(msg)
		if err != nil {
			return
		}
	}

	message := amqp.Publishing{}
	message.Body = jsonMsg
	message.Type = string(messageType)
	message.UserId = s.serverID
	message.DeliveryMode = amqp.Persistent

	exchangeName := exchange
	routingKeyString := "management_service_client"
	dConfirmation, err := s.sbpClientPub.PublishWithDeferredConfirmWithContext(
		context.Background(),
		exchangeName,
		routingKeyString,
		false,
		false,
		message,
	)
	if err != nil {
		return err
	}
	select {
	case <-time.After(time.Second * 10):
		return app.ErrSendTimeout
	case <-dConfirmation.Done():
		fmt.Println("producer: delivered deferred confirm to handler")
		break
	}
	return err
}

func (s *Service) msgStatusReport(v app.StatusReport) StatusReport {
	var stationStatus []StationStatus
	for i := range v.Stations {
		stationStatus = append(stationStatus, msgStationStatus(v.Stations[i]))
	}

	return StatusReport{
		KasseInfo:    v.KasseInfo,
		KasseStatus:  msgStatus(v.KasseStatus),
		LCWInfo:      v.LCWInfo,
		Stations:     stationStatus,
		SbpStatus:    msgServiceStatus(v.SbpStatus),
		BonusStatus:  msgServiceStatus(v.BonusStatus),
		MngtStatus:   msgServiceStatus(v.MngtStatus),
		WashServerID: s.serverID,
	}
}

func msgServiceStatus(v app.ServiceStatus) ServiceStatus {
	status := ServiceStatus{
		Available:        v.Available,
		DisabledOnServer: v.DisabledOnServer,
		IsConnected:      v.IsConnected,
		LastErr:          v.LastErr,
		UnpaidStations:   v.UnpaidStations,
		DateLastErr:      v.DateLastErr,
		ReconnectCount:   v.ReconnectCount,
	}
	return status
}

func msgStationStatus(v app.StationStatus) StationStatus {
	return StationStatus{
		ID:             int(v.ID),
		Info:           v.Info,
		Name:           v.Name,
		Status:         msgStatus(v.Status),
		CurrentBalance: v.CurrentBalance,
		CurrentProgram: v.CurrentProgram,
		ProgramName:    v.ProgramName,
		IP:             v.IP,
	}
}

func msgStatus(v app.Status) Status {
	switch v {
	case app.StatusOffline:
		return StatusOffline
	case app.StatusOnline:
		return StatusOnline
	default:
		panic(fmt.Sprintf("unknown status %v", v))
	}
}
