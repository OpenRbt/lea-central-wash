package rsutil

type RS485Config struct {
	PortName           string
	CommunicationSpeed uint
	DefaultMotorSpeed  int
}

func NewRS485Config(portName string, communicationSpeed uint, defaultMotorSpeed int) RS485Config {
	return RS485Config{
		PortName:           portName,
		CommunicationSpeed: communicationSpeed,
		DefaultMotorSpeed:  defaultMotorSpeed,
	}
}
