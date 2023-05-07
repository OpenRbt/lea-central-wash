-- +goose Up
-- SQL in this section is executed when the migration is applied.

create table rabbit_send_log
(
    id           bigserial          not null
        constraint rabbit_send_log_pk
            primary key,
    routing_key  text,
    target       text               not null,
    message_type text               not null,
    payload      jsonb              not null,
    created_at   timestamp          not null,
    sent         bool default false not null,
    sent_at      timestamp
);

create index rabbit_send_log_sent_target_index
    on rabbit_send_log (sent, target);


-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

drop table rabbit_send_log;