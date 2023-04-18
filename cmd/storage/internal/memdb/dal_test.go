package memdb

import (
	"testing"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/check"
)

func TestSmoke(tt *testing.T) {
	t := check.T(tt)
	err := db.Save(1, "key1", "value1")
	t.Nil(err)
	err = db.Save(1, "key2", "value2")
	t.Nil(err)
	err = db.Save(2, "key1", "value4")
	t.Nil(err)

	v, err := db.Load(1, "key2")
	t.Nil(err)
	t.DeepEqual(v, "value2")
	v, err = db.Load(2, "key1")
	t.Nil(err)
	t.DeepEqual(v, "value4")

	err = db.Save(1, "key1", "value5")
	t.Nil(err)
	v, err = db.Load(1, "key1")
	t.Nil(err)
	t.DeepEqual(v, "value5")

	_, err = db.Load(1, "key3")
	t.Err(err, app.ErrNotFound)

	t.Equal(db.Info(), "memdb")
}
