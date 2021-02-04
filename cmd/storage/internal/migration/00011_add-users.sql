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

INSERT INTO users (first_name, middle_name, last_name, password, is_admin)
VALUES ('Иван', 'Иванович', 'Иванов', '$2y$10$8yv.lzXgxF8PGLjDecCqsuy3aVqRCRQWUxHgDPQb9QW23Dpk2eY/.', true);

ALTER TABLE money_collection ADD user_id INT REFERENCES users(id); 

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE users;
