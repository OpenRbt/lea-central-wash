-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE public.openwashing_logs (
	id          BIGSERIAL PRIMARY KEY,
	station_id  INT       NOT NULL REFERENCES station(id),
	text        TEXT      NOT NULL,
	type        TEXT,
	created_at  TIMESTAMP NOT NULL DEFAULT NOW(),
	management_sended BOOLEAN NOT NULL DEFAULT false
);


-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE IF EXISTS public.openwashing_logs;
