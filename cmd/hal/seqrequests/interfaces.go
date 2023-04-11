package seqrequests

type PortReporter interface {
	FreePort(port string)
}
