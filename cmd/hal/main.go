package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/api/xgrpc"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/hwmetrics"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/service"

	"google.golang.org/grpc"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	apiHardware   app.HardwareAccessLayer
	motorDetected = promauto.NewGaugeVec(prometheus.GaugeOpts{
		Name: "hal_motor_detected",
		Help: "Motor detected",
	},
		[]string{"id"})
	boardDetected = promauto.NewGaugeVec(prometheus.GaugeOpts{
		Name: "hal_board_detected",
		Help: "Motor detected",
	},
		[]string{"id"})
	motorSpeedGauge = promauto.NewGaugeVec(prometheus.GaugeOpts{
		Name: "hal_motor_speed",
		Help: "Motor speed",
	},
		[]string{"id"})
	motorDesiredSpeedGauge = promauto.NewGaugeVec(prometheus.GaugeOpts{
		Name: "hal_motor_desired_speed",
		Help: "Motor speed",
	},
		[]string{"id"})
	motorRunning = promauto.NewGaugeVec(prometheus.GaugeOpts{
		Name: "hal_motor_running",
		Help: "zero for stopped motor, 1 for running",
	},
		[]string{"id"})
	rs485MotorRequestCounter = promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "hal_motor_requests",
		Help: "Total number of requests",
	},
		[]string{"id"})
	rs485MotorRequestFailCounter = promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "hal_motorfail_requests",
		Help: "Total number of failed requests",
	},
		[]string{"motor_id"})
	rev2BoardReconnectCounter = promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "hal_rev2_board_reconnect",
		Help: "Total number of reconnect",
	},
		[]string{"post_id"})
	rev1DispencerReconnectCounter = promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "hal_rev1_dispencer_reconnect",
		Help: "Total number of reconnect",
	},
		[]string{"id"})
)

func main() {
	isDebug := flag.Bool("debug", false, "debug")

	metricsHW := app.HardwareMetrics{
		MotorDetected:                 hwmetrics.NewGaugeMetric(motorDetected),
		BoardDetected:                 hwmetrics.NewGaugeMetric(boardDetected),
		MotorSpeedGauge:               hwmetrics.NewGaugeMetric(motorSpeedGauge),
		MotorDesiredSpeedGauge:        hwmetrics.NewGaugeMetric(motorDesiredSpeedGauge),
		MotorRunning:                  hwmetrics.NewGaugeMetric(motorRunning),
		RS485MotorRequestCounter:      hwmetrics.NewCounterMetric(rs485MotorRequestCounter),
		RS485MotorRequestFailCounter:  hwmetrics.NewCounterMetric(rs485MotorRequestFailCounter),
		Rev2BoardReconnectCounter:     hwmetrics.NewCounterMetric(rev2BoardReconnectCounter),
		Rev1DispencerReconnectCounter: hwmetrics.NewCounterMetric(rev1DispencerReconnectCounter),
	}

	var hardware app.HardwareAccessLayer
	var errHardware error
	if *isDebug {
		hardware, errHardware = service.NewHardwareDebugAccessLayer()
	} else {
		hardware, errHardware = service.NewHardwareAccessLayer(metricsHW)
	}
	if errHardware != nil {
		log.Println("HARDWARE IS NOT WORKING")
	}
	apiHardware = hardware

	// Let's handle metrics
	http.Handle("/metrics", promhttp.Handler())

	// Let's handle hw info... In the next iteration it must be
	// rebuild properly, without global variables
	http.HandleFunc("/hw", handleHardwareInfo)
	go http.ListenAndServe(":2112", nil)

	halHandler := xgrpc.New(hardware)

	l, err := net.Listen("tcp", ":8099")
	if err != nil {
		log.Fatalln(err)
	}

	hardware.Start()

	s := grpc.NewServer()

	xgrpc.RegisterHardwareAccessLayerServer(s, halHandler)

	err = s.Serve(l)
	if err != nil {
		log.Fatalln(err)
	}
}

func handleHardwareInfo(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "%+v", apiHardware.DeviceInfo())
}
