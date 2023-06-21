-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE money_report
    add column qr_money integer not null default 0;

ALTER TABLE money_collection
    add column qr_money integer not null default 0;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE money_report
    drop column qr_money;

ALTER TABLE money_collection
    drop column qr_money;