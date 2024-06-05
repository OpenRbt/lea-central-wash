-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TYPE LOG_LEVEL_ENUM AS ENUM ('debug', 'info', 'warning', 'error');
ALTER TABLE openwashing_logs ADD COLUMN level LOG_LEVEL_ENUM DEFAULT 'info';

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE openwashing_logs DROP COLUMN level;
DROP TYPE IF EXISTS LOG_LEVEL_ENUM;
