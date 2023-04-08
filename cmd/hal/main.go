package main

import (
	"flag"
	"log"
	"net"

	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/api/xgrpc"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/hal/service"

	"google.golang.org/grpc"
)

func main() {
	isDebug := flag.Bool("debug", false, "debug")

	var hardware app.HardwareAccessLayer
	var errHardware error
	if *isDebug {
		hardware, errHardware = service.NewHardwareDebugAccessLayer()
	} else {
		hardware, errHardware = service.NewHardwareAccessLayer()
	}
	if errHardware != nil {
		log.Println("HARDWARE IS NOT WORKING")
	}

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
