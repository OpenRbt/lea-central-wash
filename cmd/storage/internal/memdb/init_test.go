package memdb

var db *DB

func init() {
	db = New()
}
