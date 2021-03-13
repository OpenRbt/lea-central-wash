-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE update_config (
    id                  SERIAL      PRIMARY KEY,
    ctime               TIMESTAMP NOT NULL DEFAULT NOW(),
    note                TEXT        NOT NULL DEFAULT ''
);

INSERT INTO update_config (note) VALUES('init');

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE update_config;