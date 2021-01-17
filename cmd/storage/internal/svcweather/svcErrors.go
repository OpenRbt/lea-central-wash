package svcweather

import (
	"errors"
	"fmt"
)

// ErrNoPropertyFound type
type ErrNoPropertyFound struct {
	URL      string
	Property string
}

// ErrBadStatusCode type
type ErrBadStatusCode struct {
	URL        string
	StatusCode int
	Reason     string
}

// ErrFailedHTTPRequest type
type ErrFailedHTTPRequest struct {
	URL string
}

// ErrMissingEnvironmentVariable type
type ErrMissingEnvironmentVariable struct {
	Name string
}

// ErrNoPayload type
type ErrNoPayload struct {
	URL string
}

// Error returns a new ErrPropertyFound
func (err *ErrNoPropertyFound) Error() error {
	message := fmt.Sprintf("No property %s found in the response from %s", err.Property, err.URL)
	return errors.New(message)
}

// Error returns a new ErrBadStatusCode
func (err *ErrBadStatusCode) Error() error {
	message := fmt.Sprintf("Bad status code %d returned by %s, reason: %s", err.StatusCode, err.URL, err.Reason)
	return errors.New(message)
}

// Error returns a new ErrFailedHTTPRequest
func (err *ErrFailedHTTPRequest) Error() error {
	message := fmt.Sprintf("Failed HTTP request to URL: %s", err.URL)
	return errors.New(message)
}

// Error returns a new ErrMissingEnvironmentVariable
func (err *ErrMissingEnvironmentVariable) Error() error {
	message := fmt.Sprintf("Missing an environment variable %s", err.Name)
	return errors.New(message)
}

// Error returns a new ErrNoPayload
func (err *ErrNoPayload) Error() error {
	message := fmt.Sprintf("No payload returned by URL: %s", err.URL)
	return errors.New(message)
}
