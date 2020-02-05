package memdb

import (
	"sync"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

type keypair struct {
	StationID string
	Key       string
	Value     []byte
}

type DB struct {
	keypair []keypair
	mutex   sync.Mutex
}

// New DB
func New() *DB {
	return &DB{keypair: []keypair{}}
}

func (t *DB) Load(stationID string, key string) ([]byte, error) {
	t.mutex.Lock()
	defer t.mutex.Unlock()

	i := t.findKey(stationID, key)
	if i < 0 {
		return nil, app.ErrNotFound
	}
	return t.keypair[i].Value, nil
}

func (t *DB) Save(stationID string, key string, value []byte) (err error) {
	t.mutex.Lock()
	defer t.mutex.Unlock()

	i := t.findKey(stationID, key)
	if i < 0 {
		t.keypair = append(t.keypair, keypair{
			StationID: stationID,
			Key:       key,
			Value:     value,
		})
		return nil
	}
	t.keypair[i].Value = value
	return nil
}

func (t *DB) findKey(stationID string, key string) int {
	for i, _ := range t.keypair {
		if t.keypair[i].StationID == stationID && t.keypair[i].Key == key {
			return i
		}
	}
	return -1
}

// Info returns database information
func (t *DB) Info() string {
	return "memdb"
}
