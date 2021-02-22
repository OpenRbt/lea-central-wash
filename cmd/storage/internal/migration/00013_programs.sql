-- +goose Up
-- SQL in this section is executed when the migration is applied.

DROP TABLE station_program;

CREATE TABLE program (
    id                  INT         PRIMARY KEY,
    name                TEXT        NOT NULL DEFAULT '',
    price               INT         NOT NULL DEFAULT 0,
    note                TEXT        NOT NULL DEFAULT '',
    relays              TEXT        NOT NULL DEFAULT '[]',
    preflight_relays    TEXT        NOT NULL DEFAULT '[]',
    preflight_enabled   BOOLEAN     NOT NULL DEFAULT FALSE
);

CREATE TABLE station_program (
    station_id      INT         NOT NULL REFERENCES station(id),
    program_id      INT         NOT NULL REFERENCES program(id),
    button_id       INT         NOT NULL DEFAULT 0,
    PRIMARY KEY (station_id, button_id)
);

ALTER TABLE station ADD COLUMN preflight_sec INT NOT NULL DEFAULT 0;
-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE station_program;
DROP TABLE program;
ALTER TABLE station DROP COLUMN preflight_sec;

CREATE TABLE station_program (
    station_id      INT         NOT NULL REFERENCES station(id),
    program_id      INT         NOT NULL,
    name            TEXT        NOT NULL DEFAULT '',
    relays    TEXT        NOT NULL DEFAULT '[]',
    PRIMARY KEY (station_id, program_id)
);
