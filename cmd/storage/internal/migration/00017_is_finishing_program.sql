-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE program ADD COLUMN is_finishing_program BOOLEAN NOT NULL DEFAULT FALSE;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE program DROP COLUMN is_finishing_program;
