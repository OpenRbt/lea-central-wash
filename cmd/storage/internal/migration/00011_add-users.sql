-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE users (
id          SERIAL  PRIMARY KEY,
first_name  TEXT NOT NULL,
middle_name TEXT NOT NULL,
last_name   TEXT NOT NULL,
password    TEXT NOT NULL,
is_admin    boolean DEFAULT FALSE,
is_operator boolean DEFAULT FALSE,
is_engineer boolean DEFAULT FALSE,
ctime       timestamp default now(),
UNIQUE (first_name, middle_name, last_name)
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE users;
