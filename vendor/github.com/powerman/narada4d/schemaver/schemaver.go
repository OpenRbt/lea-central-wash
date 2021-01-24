package schemaver

import (
	"fmt"
	"net/url"
	"os"
	"sync"
)

const (
	// NoVersion will be set as version value by Initialize.
	NoVersion = "none"
	// BadVersion must be set as version value in case data schema may
	// be incorrect (as result of interrupted migration or restoring
	// backup).
	BadVersion = "dirty"
	// EnvLocation must contain url pointing to location of data
	// schema version. For example: file:///some/path/.
	EnvLocation = "NARADA4D"
	// EnvSkipLock must be set to non-empty value in case
	// ExclusiveLock was already acquired (by parent process).
	EnvSkipLock = "NARADA4D_SKIP_LOCK"
)

func location() (*url.URL, error) {
	loc, err := url.Parse(os.Getenv(EnvLocation))
	if err != nil {
		return nil, fmt.Errorf("failed to parse $%s: %v", EnvLocation, err)
	}
	if registered[loc.Scheme] == nil {
		return nil, fmt.Errorf("unknown protocol in $%s: %q", EnvLocation, loc.Scheme)
	}
	return loc, nil
}

// Initialize initialize version at location provided in $NARADA4D.
//
// Version must not be already initialized.
func Initialize() error {
	loc, err := location()
	if err != nil {
		return err
	}
	return registered[loc.Scheme].Initialize(loc)
}

type lockType int

const (
	unlocked lockType = iota
	shared
	exclusive
)

// SchemaVer manage data schema versions.
type SchemaVer struct {
	backend    Manage
	mu         sync.Mutex
	lockType   lockType
	skipUnlock int
	callbacks  []func(string)
}

// New creates object for managing data schema version at location
// provided in $NARADA4D.
//
// Version must be already initialized.
func New() (*SchemaVer, error) {
	loc, err := location()
	if err != nil {
		return nil, err
	}
	backend, err := registered[loc.Scheme].New(loc)
	if err != nil {
		return nil, err
	}

	v := &SchemaVer{
		backend: backend,
	}
	if os.Getenv(EnvSkipLock) != "" {
		v.lockType = exclusive
	}

	return v, nil
}

// SharedLock acquire shared lock and return current version.
//
// It may be called recursively, under already acquired SharedLock
// or ExclusiveLock (in this case it'll do nothing).
func (v *SchemaVer) SharedLock() string {
	v.mu.Lock()
	defer v.mu.Unlock()

	if v.lockType != unlocked {
		v.skipUnlock++
	} else {
		v.backend.SharedLock()
		v.lockType = shared
	}

	ver := v.backend.Get()
	for _, callback := range v.callbacks {
		callback(ver)
	}
	return ver
}

// ExclusiveLock acquire exclusive lock and return current version.
//
// It may be called recursively, under already acquired ExclusiveLock
// (in this case it'll do nothing).
func (v *SchemaVer) ExclusiveLock() string {
	v.mu.Lock()
	defer v.mu.Unlock()

	switch {
	case v.lockType == exclusive:
		v.skipUnlock++
	case v.lockType == shared:
		panic("unable to acquire exclusive lock under shared lock")
	default:
		v.backend.ExclusiveLock()
		v.lockType = exclusive
		if err := os.Setenv(EnvSkipLock, "1"); err != nil {
			panic(err)
		}
	}

	ver := v.backend.Get()
	for _, callback := range v.callbacks {
		callback(ver)
	}
	return ver
}

// Unlock release lock acquired using SharedLock or ExclusiveLock.
//
// When called to unlock previous SharedLock or ExclusiveLock
// which did nothing (because lock was already acquired) then it
// will do nothing too.
func (v *SchemaVer) Unlock() {
	v.mu.Lock()
	defer v.mu.Unlock()

	if v.lockType == unlocked {
		panic("can't unlock, no lock acquired")
	}
	if v.skipUnlock > 0 {
		v.skipUnlock--
	} else {
		v.backend.Unlock()
		v.lockType = unlocked
		if err := os.Unsetenv(EnvSkipLock); err != nil {
			panic(err)
		}
	}
}

// Get returns current version.
//
// It must be called under SharedLock or ExclusiveLock.
func (v *SchemaVer) Get() string {
	v.mu.Lock()
	defer v.mu.Unlock()

	if v.lockType == unlocked {
		panic("require SharedLock or ExclusiveLock")
	}
	return v.backend.Get()
}

// Set change current version.
//
// It must be called under ExclusiveLock.
func (v *SchemaVer) Set(ver string) {
	v.mu.Lock()
	defer v.mu.Unlock()

	if v.lockType != exclusive {
		panic("require ExclusiveLock")
	}
	v.backend.Set(ver)
}

// AddCallback registers user-provided function which will be
// called with current version in parameter by each SharedLock or
// ExclusiveLock before they returns.
//
// Usually this function should check is current version is
// supported by this application and call log.Fatal if not.
func (v *SchemaVer) AddCallback(callback func(string)) {
	v.mu.Lock()
	defer v.mu.Unlock()

	if callback == nil {
		panic("require callback")
	}

	v.callbacks = append(v.callbacks, callback)
}

// Close release any resources used to manage schema version.
//
// No other methods should be called after Close.
func (v *SchemaVer) Close() error {
	v.mu.Lock()
	defer v.mu.Unlock()
	return v.backend.Close()
}
