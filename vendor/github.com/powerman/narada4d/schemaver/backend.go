package schemaver

import (
	"fmt"
	"net/url"
)

// Backend used for registering backend implementing concrete protocol.
type Backend struct {
	// New returns implementation of SchemaVerBackend working with
	// version at given location or error if location is incorrect.
	New func(*url.URL) (Manage, error)
	// Initialize version at given location or return error if
	// location is incorrect or already initialized.
	Initialize func(*url.URL) error
}

// Manage interface must be implemented by concrete data schema
// version managing protocols.
type Manage interface {
	// SharedLock must acquire shared lock on version value.
	//
	// If called with already acquired lock previous lock may be
	// released before acquiring new one.
	SharedLock()
	// ExclusiveLock must acquire exclusive lock on version value.
	//
	// If called with already acquired lock previous lock may be
	// released before acquiring new one.
	ExclusiveLock()
	// Unlock will be called after SharedLock or ExclusiveLock and
	// must release lock on version value.
	Unlock()
	// Get will be called after SharedLock or ExclusiveLock and must
	// return current version.
	Get() string
	// Set will be called after ExclusiveLock and must change current
	// version.
	Set(string)
	// Close will release any resources used to manage schema version.
	Close() error
}

//nolint:gochecknoglobals
var registered = make(map[string]*Backend)

// RegisterProtocol must be called by packages which implement some
// protocol before first call to Initialize or New.
func RegisterProtocol(proto string, backend Backend) {
	if registered[proto] != nil {
		panic(fmt.Sprintf("protocol %q already registered", proto))
	} else if backend.Initialize == nil || backend.New == nil {
		panic(fmt.Sprintf("can't register protocol %q with nil implementation", proto))
	}

	registered[proto] = &backend
}
