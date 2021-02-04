-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE users (
id          SERIAL  PRIMARY KEY,
first_name  TEXT NOT NULL,
middle_name TEXT NOT NULL,
last_name   TEXT NOT NULL,
password    TEXT NOT NULL,
enabled     boolean NOT NULL,
ctime       timestamp default now(),
UNIQUE (first_name, middle_name, last_name)
);

CREATE TABLE security_roles (
role TEXT PRIMARY KEY
);

CREATE TABLE user_roles (
user_id INTEGER REFERENCES users(id),
user_role TEXT REFERENCES security_roles(role),
PRIMARY KEY (user_id, user_role)
);

INSERT INTO security_roles(role) VALUES ('ADMIN'), ('USER');

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE user_roles;
DROP TABLE users;
DROP TABLE security_roles;
