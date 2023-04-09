-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE money_report ADD COLUMN session_id text DEFAULT '';

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE money_report DROP COLUMN session_id;
