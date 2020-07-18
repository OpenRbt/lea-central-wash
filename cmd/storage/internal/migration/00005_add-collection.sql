-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE money_collection (
    id           SERIAL    PRIMARY KEY,
    station_id   INT         NOT NULL REFERENCES station(id),
    money        INT,
    ctime        timestamp default now()
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE money_collection;