-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE card_reader (
    station_id   INT         NOT NULL REFERENCES station(id),
    card_reader_type TEXT NOT NULL DEFAULT '',
    host TEXT NOT NULL DEFAULT '',
    port TEXT NOT NULL DEFAULT '',
    ctime TIMESTAMP DEFAULT NOW(),
    mtime TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY(station_id)
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE card_reader;
