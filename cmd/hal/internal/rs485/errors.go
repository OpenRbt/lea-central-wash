package rs485

import "errors"

// Errors ...
var (
	ErrDeviceNotFound = errors.New("device is not found")
	ErrAlreadyRunning = errors.New("service already running")
)
