-- +goose Up
-- SQL in this section is executed when the migration is applied.


ALTER TABLE station ADD COLUMN service_mode BOOLEAN DEFAULT FALSE;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE station DROP COLUMN workmode;