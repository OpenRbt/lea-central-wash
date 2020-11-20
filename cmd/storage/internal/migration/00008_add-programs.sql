-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE station_program (
    station_id      INT         NOT NULL REFERENCES station(id),
    program_id      INT         NOT NULL,
    name            TEXT        NOT NULL DEFAULT '',
    program_data    TEXT        NOT NULL DEFAULT '[]'
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE station_program;
