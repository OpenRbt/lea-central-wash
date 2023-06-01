-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE money_collection ADD COLUMN bonuses INT DEFAULT 0;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
ALTER TABLE money_collection DROP COLUMN bonuses;
