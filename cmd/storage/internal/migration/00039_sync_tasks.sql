-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE tasks
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE tasks
    ALTER COLUMN retry_count SET NOT NULL;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE tasks
    DROP column version,
    DROP column management_sended;

ALTER TABLE tasks
    ALTER COLUMN retry_count DROP NOT NULL;
