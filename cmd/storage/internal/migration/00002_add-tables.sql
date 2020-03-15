-- +goose Up
-- SQL in this section is executed when the migration is applied.
CREATE TABLE keypair (
    id          SERIAL      PRIMARY KEY,
    station_id  TEXT        NOT NULL,
    key         TEXT        NOT NULL,
    value       TEXT       NOT NULL,
    UNIQUE (station_id, key)
);
-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE keypair;
