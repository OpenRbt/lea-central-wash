-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE users (
id          SERIAL  PRIMARY KEY,
login       TEXT NOT NULL UNIQUE,
first_name  TEXT,
middle_name TEXT,
last_name   TEXT,
password    TEXT NOT NULL,
is_admin    boolean DEFAULT FALSE,
is_operator boolean DEFAULT FALSE,
is_engineer boolean DEFAULT FALSE,
ctime       timestamp default now()
);

INSERT INTO users (login, password, is_admin)
VALUES ('admin', '$2y$10$8yv.lzXgxF8PGLjDecCqsuy3aVqRCRQWUxHgDPQb9QW23Dpk2eY/.', true);

ALTER TABLE money_collection ADD user_id INT REFERENCES users(id); 

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
ALTER TABLE money_collection DROP user_id;
DROP TABLE users;
