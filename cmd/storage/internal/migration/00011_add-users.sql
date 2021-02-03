-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE users (
first_name TEXT NOT NULL,
middle_name TEXT NOT NULL,
last_name TEXT NOT NULL,
password TEXT NOT NULL,
enabled boolean NOT NULL,
ctime        timestamp default now(),
PRIMARY KEY (first_name, middle_name, last_name)
);

CREATE TABLE security_roles (
role TEXT PRIMARY KEY
);

CREATE TABLE user_roles (
user_first_name TEXT NOT NULL,
user_middle_name TEXT NOT NULL,
user_last_name TEXT NOT NULL,
user_role TEXT REFERENCES security_roles(role),
FOREIGN KEY (user_first_name, user_middle_name, user_last_name) references users(first_name, middle_name, last_name),
PRIMARY KEY (user_first_name, user_middle_name, user_last_name, user_role)
);

INSERT INTO security_roles(role) VALUES ('ADMIN'), ('USER');

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE user_roles;
DROP TABLE users;
DROP TABLE security_roles;
