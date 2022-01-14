-- +goose Up
-- SQL in this section is executed when the migration is applied.


CREATE TABLE advertising_campaign (
    id                  SERIAL    PRIMARY KEY,
	name                TEXT NOT NULL  DEFAULT '',
   	default_discount    INT       NOT NULL DEFAULT 0,
	discount_programs   TEXT NOT NULL  DEFAULT '[]',
	end_date            timestamp default now(),
	end_minute          INT       NOT NULL DEFAULT 0,
	start_date          timestamp default now(),
	start_minute        INT       NOT NULL DEFAULT 0,
	weekday             TEXT NOT NULL  DEFAULT '',
    enabled             BOOLEAN NOT NULL DEFAULT true,
    ctime               timestamp default now()
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE advertising_campaign;
