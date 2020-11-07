-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE open_station_log (
    id           SERIAL      PRIMARY KEY,
    station_id   INT  NOT NULL,
    ctime        timestamp default now()
);

CREATE INDEX open_station_log_station_id on open_station_log(station_id);
-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE open_station_log;
