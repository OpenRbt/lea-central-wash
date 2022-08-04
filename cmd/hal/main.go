package main

import (
	"flag"
	"fmt"
	"hal/internal/api/xgrpc"
	"hal/internal/app"
	"hal/service"
	"log"
	"net"

	"google.golang.org/grpc"
)

func main() {
	fmt.Println("HAL START, AVE ME!!!")
	isDebug := flag.Bool("debug", false, "debug")

	var hardware app.HardwareAccessLayer
	var arduinoware app.HardwareArduinoAccessLayer
	var errHardware error
	var errArduino error
	if *isDebug {
		hardware, errHardware = service.NewHardwareDebugAccessLayer()
	} else {
		hardware, errHardware = service.NewHardwareAccessLayer()
	}
	if errHardware != nil {
		log.Println("HARDWARE IS NOT WORKING")
	}
	arduinoware, errArduino = service.NewHardwareArduinoLayer()
	if errArduino != nil {
		log.Println("ARDUINO IS NOT WORKING")
	}
	arduinoware.Start()

	halHandler := xgrpc.New(hardware)

	l, err := net.Listen("tcp", ":8099")
	if err != nil {
		log.Fatalln(err)
	}

	// var text string
	// fmt.Scanf("%s\n", &text)
	// arduinoware.Command(text)

	hardware.Start()

	s := grpc.NewServer()

	xgrpc.RegisterHardwareAccessLayerServer(s, halHandler)

	err = s.Serve(l)
	if err != nil {
		log.Fatalln(err)
	}
}
