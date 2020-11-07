-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE station_hash (
station_id   INT  NOT NULL REFERENCES station(id),
hash         TEXT NOT NULL,
ctime        timestamp default now(),
CONSTRAINT   hash_key UNIQUE (hash),
CONSTRAINT   station_id_key UNIQUE (station_id)
);

INSERT INTO station_hash(station_id, hash) SELECT id, hash from station where hash is not null;

ALTER TABLE station DROP COLUMN hash;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE station_hash;
ALTER TABLE station ADD COLUMN hash TEXT;