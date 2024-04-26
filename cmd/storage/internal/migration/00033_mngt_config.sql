-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE station
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE station_hash
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE station_program
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE users
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE advertising_campaign
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE card_reader
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE config_vars_bool
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE config_vars_int
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE config_vars_string
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE kasse
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE keypair
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE program
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE station_config_vars_bool
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE station_config_vars_int
    add column version int not null default 0,
    add column management_sended boolean not null default false;

ALTER TABLE station_config_vars_string
    add column version int not null default 0,
    add column management_sended boolean not null default false;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
ALTER TABLE station
    DROP column version,
    DROP column management_sended;

ALTER TABLE station_hash
    DROP column version,
    DROP column management_sended;

ALTER TABLE station_program
    DROP column version,
    DROP column management_sended;

ALTER TABLE users
    DROP column version,
    DROP column management_sended;

ALTER TABLE advertising_campaign
    DROP column version,
    DROP column management_sended;

ALTER TABLE card_reader
    DROP column version,
    DROP column management_sended;

ALTER TABLE config_vars_bool
    DROP column version,
    DROP column management_sended;

ALTER TABLE config_vars_int
    DROP column version,
    DROP column management_sended;

ALTER TABLE config_vars_string
    DROP column version,
    DROP column management_sended;

ALTER TABLE kasse
    DROP column version,
    DROP column management_sended;

ALTER TABLE keypair
    DROP column version,
    DROP column management_sended;

ALTER TABLE program
    DROP column version,
    DROP column management_sended;

ALTER TABLE station_config_vars_bool
    DROP column version,
    DROP column management_sended;

ALTER TABLE station_config_vars_int
    DROP column version,
    DROP column management_sended;

ALTER TABLE station_config_vars_string
    DROP column version,
    DROP column management_sended;
