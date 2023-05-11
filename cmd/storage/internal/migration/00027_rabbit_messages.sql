-- +goose Up
-- SQL in this section is executed when the migration is applied.

create table bonus_rabbit_send_log
(
    id           bigserial          not null
        constraint bonus_rabbit_send_log_pk
            primary key,
    message_type text               not null,
    payload      jsonb              not null,
    created_at   timestamp          not null,
    sent         bool default false not null,
    sent_at      timestamp
);

create table bonus_rabbit_money_report_send_log(
   id           bigserial          not null
       constraint bonus_rabbit_money_report_send_log_pk
           primary key,
   message_type text               not null,
   money_report_id      integer not null references money_report(id),
   created_at   timestamp          not null,
   sent         bool default false not null,
   sent_at      timestamp
);

create index rabbit_send_log_sent_target_index
    on bonus_rabbit_send_log (sent);
create index bonus_rabbit_money_report_send_log_index
    on bonus_rabbit_money_report_send_log (sent);


-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

drop table bonus_rabbit_send_log;
drop table bonus_rabbit_money_report_send_log;