-- +goose Up
-- SQL in this section is executed when the migration is applied.
CREATE TABLE users (
id          SERIAL PRIMARY KEY,
login       TEXT NOT NULL,
first_name  TEXT NOT NULL,
middle_name TEXT NOT NULL,
last_name   TEXT NOT NULL,
password    TEXT NOT NULL,
is_admin    boolean NOT NULL,
is_operator boolean NOT NULL,
is_engineer boolean NOT NULL,
ctime       timestamp default now()
);

CREATE UNIQUE INDEX users_unique_lower_login_idx ON users(LOWER(login));

INSERT INTO users (login, first_name, middle_name, last_name, password, is_admin, is_operator, is_engineer)
VALUES ('admin', '', '', '', '$2y$10$8yv.lzXgxF8PGLjDecCqsuy3aVqRCRQWUxHgDPQb9QW23Dpk2eY/.', true, false, false);

ALTER TABLE money_collection ADD user_id INT REFERENCES users(id); 

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
ALTER TABLE money_collection DROP user_id;
DROP TABLE users;
