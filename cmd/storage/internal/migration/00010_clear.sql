-- +goose Up
-- SQL in this section is executed when the migration is applied.

TRUNCATE TABLE money_report;

DROP TABLE money_collection;

CREATE TABLE money_collection (
    id           SERIAL      PRIMARY KEY,
    station_id   INT         NOT NULL REFERENCES station(id),
    banknotes    INT NOT NULL DEFAULT 0,
	cars_total   INT NOT NULL DEFAULT 0,
	coins        INT NOT NULL DEFAULT 0,
	electronical INT NOT NULL DEFAULT 0,
	service      INT NOT NULL DEFAULT 0,
    last_money_report_id INT  NOT NULL DEFAULT 0,
    ctime        timestamp default now()
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE money_collection;

CREATE TABLE money_collection (
    id           SERIAL    PRIMARY KEY,
    station_id   INT         NOT NULL REFERENCES station(id),
    money        INT,
    ctime        timestamp default now()
);
