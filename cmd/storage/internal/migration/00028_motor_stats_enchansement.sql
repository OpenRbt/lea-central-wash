-- +goose Up
-- SQL in this section is executed when the migration is applied.
CREATE INDEX relay_stat_relay_report_id_id_index on relay_stat (relay_report_id, id);
CREATE INDEX relay_report_id_station_id_index on relay_report(id,station_id);

CREATE MATERIALIZED VIEW mv_relay_stat_dates as
SELECT p.station_id,
       r.relay_id,
       sum(r.switched_count)      as switched_count,
       sum(r.total_time_on)       as total_time_on,
       date_trunc('day', p.ctime) as date
FROM relay_stat r
         JOIN relay_report p on r.relay_report_id = p.id
Group BY p.station_id, r.relay_id, date
ORDER BY p.station_id,r.relay_id;

create index on mv_relay_stat_dates (station_id,date);

CREATE MATERIALIZED VIEW mv_program_stat_dates as
SELECT r.station_id,
       r.program_id,
       coalesce(p.name, '')       as program_name,
       sum(r.time_on)             as time_on,
       sum(r.pump_time_on)        as pump_time_on,
       date_trunc('day', r.ctime) as date
FROM relay_report r
         LEFT JOIN program p on p.id = r.program_id
GROUP BY r.station_id, r.program_id, coalesce(p.name, ''), date
ORDER BY r.station_id, r.program_id;

create index on mv_program_stat_dates (station_id,date);

CREATE MATERIALIZED VIEW mv_current_relay_stat as
SELECT p.station_id, r.relay_id, sum(r.switched_count) as switched_count, sum(r.total_time_on) as total_time_on
FROM relay_stat r
         JOIN relay_report p on r.relay_report_id = p.id
WHERE p.id > coalesce(
        (SELECT last_relay_report_id
         FROM reset_relay_report
         WHERE station_id = p.station_id
         ORDER BY id DESC
         LIMIT 1)
    , 0)
GROUP BY p.station_id, r.relay_id
ORDER BY p.station_id, r.relay_id;

CREATE MATERIALIZED VIEW mv_current_program_stat as
SELECT r.station_id,
       r.program_id,
       coalesce(p.name, '') as program_name,
       sum(r.time_on)       as time_on,
       sum(r.pump_time_on)  as pump_time_on
FROM relay_report r
         LEFT JOIN program p on p.id = r.program_id
WHERE r.id > coalesce(
        (SELECT last_relay_report_id
         FROM reset_relay_report
         WHERE station_id = r.station_id
         ORDER BY id DESC
         LIMIT 1)
    , 0)
GROUP BY r.station_id, r.program_id, coalesce(p.name, '')
ORDER BY r.station_id, r.program_id;
-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP INDEX relay_stat_relay_report_id_id_index;
DROP INDEX relay_report_id_station_id_index;

DROP MATERIALIZED VIEW mv_relay_stat_dates;
DROP MATERIALIZED VIEW mv_program_stat_dates;
DROP MATERIALIZED VIEW mv_current_relay_stat;
DROP MATERIALIZED VIEW mv_current_program_stat;
