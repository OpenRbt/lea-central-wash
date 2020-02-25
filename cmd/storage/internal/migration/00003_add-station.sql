-- +goose Up
-- SQL in this section is executed when the migration is applied.
DROP TABLE keypair;

CREATE TABLE station (
    id          SERIAL    PRIMARY KEY,
    hash        TEXT,
    name        TEXT      NOT NULL,
    deleted     BOOLEAN   DEFAULT FALSE,
	UNIQUE (hash),
	UNIQUE (name)
);

CREATE TABLE keypair (
    id          SERIAL      PRIMARY KEY,
    station_id  INT         NOT NULL REFERENCES station(id),
    key         TEXT        NOT NULL,
    value       BYTEA       NOT NULL,
    UNIQUE (station_id, key)
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE keypair;
DROP TABLE station;

CREATE TABLE keypair (
    id          SERIAL      PRIMARY KEY,
    station_id  TEXT        NOT NULL,
    key         TEXT        NOT NULL,
    value       BYTEA       NOT NULL,
    UNIQUE (station_id, key)
);