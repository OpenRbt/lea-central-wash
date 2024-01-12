-- +goose Up
-- SQL in this section is executed when the migration is applied.

-- DROP FUNCTION IF EXISTS public.id_generator();

-- +goose StatementBegin
CREATE OR REPLACE FUNCTION uuid_generator(
	OUT result uuid)
    RETURNS uuid
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
AS $BODY$
    DECLARE
    BEGIN
        result := uuid_in(("overlay"("overlay"(md5((((random())::text || ':'::text) || (clock_timestamp())::text))
        , '4'::text, 13), to_hex((floor(((random() * (((11 - 8) + 1))::double precision) + (8)::double precision)))
        ::integer), 17))::cstring);
    END;
    
$BODY$;
-- +goose StatementEnd

ALTER TABLE money_report
    add column management_message_id uuid not null unique default uuid_generator(),
    add column management_sended boolean not null default false;

ALTER TABLE money_collection
    add column management_id uuid not null unique default uuid_generator(),
    add column management_sended boolean not null default false;

create index money_report_management_sended_ind on money_report(management_sended);
create index money_collection_management_sended_ind on money_collection(management_sended);

INSERT INTO money_collection(
	id, station_id, banknotes, cars_total, coins, electronical, service, last_money_report_id, ctime, user_id, bonuses, qr_money)
	select -id, id,0,0,0,0,0,0,to_timestamp(0),1,0,0 from station;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP index money_report_management_sended_ind;
DROP index money_collection_management_sended_ind;

ALTER TABLE money_report
    DROP column management_message_id,
    DROP column management_sended;

ALTER TABLE money_collection
    DROP column management_id,
    DROP column management_sended;

DROP FUNCTION uuid_generator;
