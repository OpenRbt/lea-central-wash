-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE station ADD COLUMN relay_board TEXT NOT NULL DEFAULT 'localGPIO';
ALTER TABLE program ADD COLUMN motor_speed_percent INT NOT NULL DEFAULT 0;
ALTER TABLE program ADD COLUMN preflight_motor_speed_percent INT NOT NULL DEFAULT 0;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE station DROP COLUMN relay_board;
ALTER TABLE program DROP COLUMN motor_speed_percent;
ALTER TABLE program DROP COLUMN preflight_motor_speed_percent;
