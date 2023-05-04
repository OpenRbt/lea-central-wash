-- +goose Up
-- SQL in this section is executed when the migration is applied.

create index money_report_station_ctime_index
    on money_report (station_id, ctime);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

drop index money_report_station_ctime_index;