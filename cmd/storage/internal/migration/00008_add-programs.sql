-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE station_program (
    station_id      INT         NOT NULL REFERENCES station(id),
    program_id      INT         NOT NULL,
    name            TEXT        NOT NULL DEFAULT '',
    relays    TEXT        NOT NULL DEFAULT '[]'
    PRIMARY KEY (station_id, program_id)
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE station_program;
