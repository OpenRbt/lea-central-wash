-- +goose Up
-- SQL in this section is executed when the migration is applied.
CREATE TABLE station_events
(
    id          SERIAL          NOT NULL,
    station_id     INT             NOT NULL,
    ctime       TIMESTAMP       NOT NULL,
    module      TEXT            NOT NULL,
    status      TEXT            NOT NULL,
    info        TEXT,
    PRIMARY KEY (id)
);
-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE station_events;