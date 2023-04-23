package requester

type PortReporter interface {
	FreePort(port string)
}
