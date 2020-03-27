-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE money_report (
    id           SERIAL    PRIMARY KEY,
    station_id   INT         NOT NULL REFERENCES station(id),
    banknotes    INT,
	cars_total   INT,
	coins        INT,
	electronical INT,
	service      INT,
    ctime        timestamp default now()
);

CREATE TABLE relay_report (
    id           SERIAL    PRIMARY KEY,
    station_id   INT       NOT NULL REFERENCES station(id),
    ctime        timestamp default now()
);

CREATE TABLE relay_stat (
    id               SERIAL    PRIMARY KEY,
    relay_report_id  INT       NOT NULL REFERENCES relay_report(id),
	relay_id         int,
	switched_count   int,
	total_time_on    int
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE money_report;
DROP TABLE relay_stat;
DROP TABLE relay_report;
