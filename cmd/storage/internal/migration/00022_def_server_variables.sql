-- +goose Up
-- SQL in this section is executed when the migration is applied.

INSERT INTO station_config_vars_int (name, value, description, note, station_id) VALUES ('VOLUME_COEF', 100, 'Volume coef', 'volume coef', 1) ON CONFLICT DO NOTHING;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

TRUNCATE TABLE station_config_vars_int;