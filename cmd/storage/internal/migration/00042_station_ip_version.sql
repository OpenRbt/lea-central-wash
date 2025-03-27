-- +goose Up
-- SQL in this section is executed when the migration is applied.

UPDATE station
	SET management_sended = false, version = station.version + 1;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
