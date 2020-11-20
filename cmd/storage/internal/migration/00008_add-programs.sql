-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE station_programs (
    id              SERIAL      PRIMARY KEY,
    station_id      INT         NOT NULL REFERENCES station(id),
    program_id      INT         NOT NULL,
    program_name    TEXT         NOT NULL,
    program_data    TEXT
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE station_programs;
