-- +goose Up
-- SQL in this section is executed when the migration is applied.

create index money_report_ctime_index
    on money_report (ctime);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

drop index money_report_ctime_index;