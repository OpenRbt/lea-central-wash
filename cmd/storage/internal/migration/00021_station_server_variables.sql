-- +goose Up
-- SQL in this section is executed when the migration is applied.


CREATE TABLE station_config_vars_int (
    name TEXT NOT NULL,
    value INTEGER,
    description TEXT,
    note TEXT,
    stationID INT NOT NULL REFERENCES station(id),
    PRIMARY KEY (name, stationID)
);

CREATE UNIQUE INDEX station_config_vars_int_unique_idx on station_config_vars_int (upper(name), stationID);

CREATE TABLE station_config_vars_bool (
    name TEXT NOT NULL,
    value BOOLEAN,
    description TEXT,
    note TEXT,
    stationID INT NOT NULL REFERENCES station(id),
    PRIMARY KEY (name, stationID)
);

CREATE UNIQUE INDEX station_config_vars_bool_unique_idx on station_config_vars_bool (upper(name), stationID);

CREATE TABLE station_config_vars_string (
    name TEXT NOT NULL,
    value TEXT,
    description TEXT,
    note TEXT,
    stationID INT NOT NULL REFERENCES station(id),
    PRIMARY KEY (name, stationID)
);

CREATE UNIQUE INDEX station_config_vars_string_unique_idx on station_config_vars_string (upper(name), stationID);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE station_config_vars_int;
DROP TABLE station_config_vars_bool;
DROP TABLE station_config_vars_string;
