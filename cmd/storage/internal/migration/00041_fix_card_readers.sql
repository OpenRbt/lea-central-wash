-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE card_reader
    DROP column version,
    DROP column management_sended;

UPDATE station
	SET management_sended = false;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE card_reader
    add column version int not null default 0,
    add column management_sended boolean not null default false;
