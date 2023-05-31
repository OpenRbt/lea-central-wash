-- +goose Up
-- SQL in this section is executed when the migration is applied.

alter table bonus_rabbit_money_report_send_log add column message_uuid uuid not null;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

alter table bonus_rabbit_money_report_send_log drop column message_uuid;