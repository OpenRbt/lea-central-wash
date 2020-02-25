package dal

const (
	sqlSetValue = `
INSERT INTO keypair (station_id, key, value)  
VALUES 	(:station_id, :key, :value)
ON CONFLICT (station_id, key)
DO
	UPDATE SET value = :value
	`
	sqlGetValue = `
SELECT value  FROM keypair  WHERE station_id = :station_id and key = :key
	`
	sqlAddStation = `
INSERT INTO station (hash, name)  
VALUES 	(:hash, :name)
	`
	sqlUpdStation = `
UPDATE station
SET hash = :hash, name = :name
WHERE id = :id
	`
	sqlGetStation = `
SELECT id, hash, name  FROM station where deleted = false ORDER BY id
	`
	sqlStationNullHash = `
UPDATE station
SET hash = null
WHERE hash = :hash
	`
	sqlDelStation = `
UPDATE station
SET deleted = true, hash = null
WHERE id = :id
	`
)

type (
	argSetValue struct {
		StationID int
		Key       string
		Value     []byte
	}
	argGetValue struct {
		StationID int
		Key       string
	}
	argAddStation struct {
		Hash string
		Name string
	}
	argUpdStation struct {
		ID   int
		Hash string
		Name string
	}
	argStationNullHash struct {
		Hash string
	}
	argDelStation struct {
		ID int
	}
	argGetStation struct {
	}
	resGetValue struct {
		Value []byte
	}
	resStation struct {
		Hash *string
		ID   int
		Name string
	}
)
