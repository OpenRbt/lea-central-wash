-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE station_program
    DROP column version,
    DROP column management_sended,
    DROP column deleted;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE station_program
    add column version int not null default 0,
    add column management_sended boolean not null default false,
    add column deleted boolean default false not null;
