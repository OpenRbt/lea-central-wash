-- +goose Up
-- SQL in this section is executed when the migration is applied.

DROP TABLE relay_stat;
DROP TABLE relay_report;

CREATE TABLE relay_report (
    id           SERIAL    PRIMARY KEY,
    station_id   INT       NOT NULL REFERENCES station(id),
    program_id   INT       NOT NULL DEFAULT 0,
    time_on      INT       NOT NULL DEFAULT 0,
    pump_time_on INT       NOT NULL DEFAULT 0,
    ctime        timestamp default now()
);

CREATE TABLE relay_stat (
    id               SERIAL    PRIMARY KEY,
    relay_report_id  INT       NOT NULL REFERENCES relay_report(id),
	relay_id         int,
	switched_count   int,
	total_time_on    int
);

CREATE TABLE reset_relay_report (
    id                      SERIAL    PRIMARY KEY,
    station_id   INT        NOT NULL REFERENCES station(id),
    last_relay_report_id    INT       NOT NULL REFERENCES relay_report(id),
    ver                     INT       NOT NULL DEFAULT 0,
    data                    TEXT NOT NULL  DEFAULT '',
    ctime                   timestamp default now() 
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE reset_relay_report;
DROP TABLE relay_stat;
DROP TABLE relay_report;

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
