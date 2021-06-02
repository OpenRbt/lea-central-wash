-- +goose Up
-- SQL in this section is executed when the migration is applied.


ALTER TABLE station ADD COLUMN service_mode BOOLEAN DEFAULT false;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE station DROP COLUMN service_mode;