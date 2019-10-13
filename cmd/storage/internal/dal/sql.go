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
)

type (
	argSetValue struct {
		StationID string
		Key       string
		Value     []byte
	}
	argGetValue struct {
		StationID string
		Key       string
	}
	resGetValue struct {
		Value []byte
	}
)
