-- +goose Up
-- SQL in this section is executed when the migration is applied.


CREATE TABLE config_vars_int (
    name TEXT NOT NULL  PRIMARY KEY,
    value INTEGER,
    description TEXT,
    note TEXT
);

CREATE UNIQUE INDEX config_vars_int_unique_idx on config_vars_int (upper(name));

CREATE TABLE config_vars_bool (
    name TEXT NOT NULL  PRIMARY KEY,
    value BOOLEAN,
    description TEXT,
    note TEXT
);

CREATE UNIQUE INDEX config_vars_bool_unique_idx on config_vars_bool (upper(name));

CREATE TABLE config_vars_string (
    name TEXT NOT NULL  PRIMARY KEY,
    value TEXT,
    description TEXT,
    note TEXT
);

CREATE UNIQUE INDEX config_vars_string_unique_idx on config_vars_string (upper(name));

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE config_vars_int;
DROP TABLE config_vars_bool;
DROP TABLE config_vars_string;
