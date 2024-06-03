package dal

import (
	"time"

	"github.com/lib/pq"
	uuid "github.com/satori/go.uuid"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
)

const (
	constraintCardReaderStationID    = "card_reader_station_id_fkey"
	constraintUserLogin              = "users_unique_lower_login_idx"
	constraintMoneyCollection        = "money_collection_user_id_fkey"
	constraintStationProgramID       = "station_program_program_id_fkey"
	constraintStationStationID       = "station_program_station_id_fkey"
	constraintStationProgramUnique   = "station_program_pkey"
	constraintStationIntStationID    = "station_config_vars_int_station_id_fkey"
	constraintStationBoolStationID   = "station_config_vars_bool_station_id_fkey"
	constraintStationStringStationID = "station_config_vars_string_station_id_fkey"
)

const (
	sqlCheckDB = `
SELECT count(column_name) as count_columns
FROM information_schema.columns 
WHERE table_name = 'station_hash'
	`
	sqlOpenStationLogAdd = `
INSERT INTO open_station_log (station_id)  
VALUES 	(:station_id)
	`
	sqlSetValue = `
INSERT INTO keypair (station_id, key, value)  
VALUES 	(:station_id, :key, :value)
ON CONFLICT (station_id, key)
DO
	UPDATE SET value = :value
	`
	sqlSetValueIfNotExists = `
INSERT INTO keypair (station_id, key, value)  
VALUES 	(:station_id, :key, :value)
ON CONFLICT (station_id, key)
DO NOTHING
	`
	sqlGetValue = `
SELECT value  FROM keypair  WHERE station_id = :station_id and key = :key
	`
	sqlGetStationsVariables = `
	SELECT s.id, s.name, k.key, k.value
	FROM station s
	JOIN keypair k on s.id = k.station_id
	ORDER BY s.id, k.key
	`
	sqlAddStation = `
INSERT INTO station (name)  
VALUES 	(:name)
RETURNING ID
	`
	sqlAddStationHash = `
INSERT INTO station_hash (station_id, hash)  
VALUES 	(:station_id, :hash)
	`
	sqlUpdStation = `
UPDATE station
SET name = :name, preflight_sec = :preflight_sec, relay_board = :relay_board
WHERE id = :id
	`
	sqlGetStations = `
	SELECT s.id, s.name, s.preflight_sec, s.relay_board, h.hash FROM station s
	LEFT JOIN station_hash h on h.station_id=s.id
	where s.deleted = false ORDER BY id
	`
	sqlGetStation = `
SELECT id, name, preflight_sec, relay_board FROM station where deleted = false and id = :id ORDER BY id
	`

	sqlGetUsers = `
SELECT 	id, 
		login,
		first_name, 
		middle_name, 
		last_name, 
		password, 
		is_admin, 
		is_operator, 
		is_engineer 
FROM users
ORDER BY last_name, first_name, middle_name
	`

	sqlGetUser = `
SELECT 	id, 
		login,
		first_name, 
		middle_name, 
		last_name, 
		password, 
		is_admin, 
		is_operator, 
		is_engineer 
FROM users 
WHERE login = lower(:login)
	`

	sqlAddUser = `
INSERT INTO users(login, first_name, middle_name, last_name, password, is_admin, is_operator, is_engineer)
VALUES (lower(:login), :first_name, :middle_name, :last_name, :password, :is_admin, :is_operator, :is_engineer)
RETURNING 	id, 
			login,
			first_name, 
			middle_name, 
			last_name, 
			password, 
			is_admin, 
			is_operator, 
			is_engineer 
	`

	sqlUpdateUser = `
UPDATE users
SET first_name = :first_name, 
	middle_name = :middle_name, 
	last_name = :last_name, 
	is_admin = :is_admin, 
	is_operator = :is_operator, 
	is_engineer = :is_engineer
WHERE login = lower(:login)
RETURNING 	id, 
			login,
			first_name, 
			middle_name, 
			last_name, 
			password,
			is_admin, 
			is_operator, 
			is_engineer
	`

	sqlUpdateUserPassword = `
UPDATE users
SET password = :new_password 
WHERE login = lower(:login)
RETURNING 	id, 
			login,
			first_name, 
			middle_name, 
			last_name, 
			password,
			is_admin, 
			is_operator, 
			is_engineer
	`

	sqlDelUser = `
DELETE FROM users WHERE login = lower(:login)
	`

	sqlLoadHash = `
SELECT station_id, hash FROM station_hash ORDER BY station_id
	`
	sqlStationNullHash = `
DELETE FROM station_hash
WHERE hash = :hash or station_id = :station_id
	`
	sqlDelStation = `
UPDATE station
SET deleted = true, hash = null
WHERE id = :id
	`
	sqlAddMoneyReport = `
INSERT INTO money_report (station_id, banknotes, cars_total, coins, electronical, service, bonuses, ctime, session_id, qr_money)  
VALUES 	(:station_id, :banknotes, :cars_total, :coins, :electronical, :service, :bonuses, :ctime, :session_id, :qr_money)
	`
	sqlAddCollectionReport = `
	INSERT INTO money_collection (station_id, user_id, banknotes, cars_total, coins, electronical, service, bonuses,qr_money, last_money_report_id, ctime) 
	(
	SELECT station_id, 
			   :user_id,
			   sum(banknotes) as banknotes, 
			   sum(cars_total) as cars_total, 
			   sum(coins) as coins, 
			   sum(electronical) as electronical, 
			   sum(service) as service,
			   sum(bonuses) as bonuses,
			   sum(qr_money) as qr_money,
			   max(id) as max_id,
			   :ctime
		FROM money_report
	WHERE station_id = :station_id and ctime > coalesce((
		SELECT ctime FROM money_collection WHERE station_id = :station_id
			ORDER BY id DESC
			LIMIT 1
		), (SELECT TIMESTAMP 'epoch')
	)
	GROUP BY station_id
	)	
	`
	sqlLastMoneyReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service, bonuses, session_id, qr_money FROM money_report WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlLastCollectionReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service, ctime, bonuses,qr_money FROM money_collection WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlCollectionReportsByDate = `
	SELECT 
		mc.station_id, 
		mc.banknotes, 
		mc.cars_total, 
		mc.coins, 
		mc.electronical, 
		mc.service,
		mc.bonuses,
		mc.ctime,
		mc.qr_money,
		COALESCE(u.login, '')  "user"
	FROM money_collection mc
	LEFT JOIN users u ON u.id = mc.user_id
	WHERE (:start_date < mc.ctime or CAST(:start_date AS TIMESTAMP) is null) AND (mc.ctime <= :end_date or CAST(:end_date AS TIMESTAMP) is null) AND  mc.station_id = :station_id
	ORDER BY mc.ctime
	`
	sqlAddRelayReport = `
INSERT INTO relay_report (station_id, program_id, time_on, pump_time_on, ctime)  
VALUES 	(:station_id, :program_id, :time_on, :pump_time_on, :ctime) RETURNING id
	`
	sqlAddRelayStat = `
INSERT INTO relay_stat (relay_report_id, relay_id, switched_count, total_time_on)
VALUES 	(:relay_report_id, :relay_id, :switched_count, :total_time_on)
	`
	sqlResetRelayStat = `
	INSERT INTO reset_relay_report (station_id, last_relay_report_id, ver, data)
	(SELECT :station_id,coalesce(max(id),0),:ver, :data
	from relay_report
	where station_id = :station_id
	GROUP BY station_id
)
	`

	sqlCurentStationReport = `
SELECT station_id, program_id, program_name, time_on, pump_time_on 
FROM mv_current_program_stat 
WHERE station_id = :station_id OR CAST(:station_id AS INT) IS NULL
`
	sqlCurentRelayStat = `
SELECT station_id, relay_id, switched_count, total_time_on
FROM mv_current_relay_stat
WHERE station_id = :station_id OR CAST(:station_id AS INT) IS NULL
	`
	sqlDatesStationReport = `
SELECT station_id, program_id, program_name, sum(time_on) as time_on, sum(pump_time_on) as pump_time_on
FROM mv_program_stat_dates
WHERE date_hours >= :start_date AND date_hours <= :end_date
  AND (station_id = :station_id or CAST(:station_id AS INT) is NULL)
GROUP BY station_id, program_id, program_name
ORDER BY station_id,program_id
`
	sqlDatesRelayStat = `
SELECT station_id, relay_id, sum(switched_count) as switched_count, sum(total_time_on) as total_time_on
FROM mv_relay_stat_dates
WHERE date_hours >= :start_date AND date_hours <= :end_date
  AND (station_id = :station_id or CAST(:station_id AS INT) is NULL)
GROUP BY station_id, relay_id
ORDER BY station_id,relay_id
	`

	sqlMoneyReport = `
	SELECT station_id, 
		   sum(banknotes) as banknotes, 
		   sum(cars_total) as cars_total, 
		   sum(coins) as coins, 
		   sum(electronical) as electronical, 
		   sum(service) as service,
		   sum(bonuses) as bonuses,
		   sum(qr_money) as qr_money
	FROM money_report
	WHERE :start_date < ctime AND ctime <= :end_date AND station_id = :station_id
	GROUP BY station_id
	`
	sqlCurrentMoney = `
	SELECT station_id,
		   sum(banknotes)    as banknotes,
		   sum(cars_total)   as cars_total,
		   sum(coins)        as coins,
		   sum(electronical) as electronical,
		   sum(service)      as service,
		   sum(bonuses)      as bonuses,
		   sum(qr_money) as qr_money
	FROM money_report
	WHERE ctime> coalesce(
			(SELECT ctime
			 FROM money_collection
			 WHERE station_id = :station_id
			 ORDER BY id DESC
			 LIMIT 1)
		, CAST('1970-01-01' AS timestamp))
	  AND station_id = :station_id
	GROUP BY station_id
	`
	sqlRelayStatReport = `
	SELECT relay_id, sum(switched_count) switched_count, sum(total_time_on) total_time_on FROM (
		(SELECT relay_id, switched_count, total_time_on 
		FROM relay_stat WHERE relay_report_id = 
			(select id from relay_report where station_id = :station_id and ctime <= :end_date
			 ORDER BY id DESC
			 LIMIT 1)
		)
		UNION ALL
		(SELECT relay_id, -switched_count, -total_time_on 
		FROM relay_stat WHERE relay_report_id = 
			(select id from relay_report where station_id = :station_id and ctime <= :start_date
			 ORDER BY id DESC
			 LIMIT 1)
		)
		) AS RR
		GROUP BY relay_id	
	`

	sqlPrograms = `
		SELECT
			id,
			price,
			name,
			preflight_enabled,
			relays,
			preflight_relays,
			motor_speed_percent,
			preflight_motor_speed_percent,
			is_finishing_program,
			version
		FROM program
		WHERE id = :id OR CAST(:id AS integer) IS NULL
		ORDER BY id ASC
	`

	sqlSetProgram = `
		INSERT INTO program (
			id,
			price,
			name,
			preflight_enabled,
			relays,
			preflight_relays,
			motor_speed_percent,
			preflight_motor_speed_percent,
			is_finishing_program
		) VALUES (
			:id,
			:price,
			:name,
			:preflight_enabled,
			:relays,
			:preflight_relays,
			:motor_speed_percent,
			:preflight_motor_speed_percent,
			:is_finishing_program
		) ON CONFLICT (id) DO UPDATE SET
			price = EXCLUDED.price,
			name = EXCLUDED.name,
			preflight_enabled = EXCLUDED.preflight_enabled,
			relays = EXCLUDED.relays,
			preflight_relays = EXCLUDED.preflight_relays,
			motor_speed_percent = EXCLUDED.motor_speed_percent,
			preflight_motor_speed_percent = EXCLUDED.preflight_motor_speed_percent,
			is_finishing_program = EXCLUDED.is_finishing_program,
			version = program.version + 1, 
			management_sended = false
	`

	sqlStationProgramAdd = `
	INSERT INTO station_program (station_id, button_id, program_id)
	VALUES (:station_id, :button_id, :program_id)
	`
	sqlStationProgram = `
	SELECT station_id, button_id, program_id
	FROM station_program
	WHERE station_id = :station_id
	`
	sqlStationConfig = `
select s.id,
	s.name,
	s.preflight_sec,
	s.relay_board,
	b.button_id,
	b.program_id,
	p.price,
	p.name as "program_name",
	p.preflight_enabled,
	p.relays,
	p.preflight_relays,
	p.motor_speed_percent,
	p.preflight_motor_speed_percent,
	p.is_finishing_program
from station s
join station_program b on s.id=b.station_id
join program p on b.program_id=p.id
WHERE s.id = :id
order by b.button_id
	`
	sqlStationProgramDel = `
	DELETE FROM station_program
	WHERE station_id = :station_id
	`

	sqlKasse = `
	SELECT receipt_item, tax_type, cashier_full_name, cashier_inn
	FROM kasse
	order by ctime desc
	limit 1
	`

	sqlSetKasse = `
	INSERT INTO kasse(receipt_item, tax_type, cashier_full_name, cashier_inn)
	VALUES (:receipt_item, :tax_type, :cashier_full_name, :cashier_inn)
	`
	sqlSetCardReaderConfig = `
	INSERT INTO card_reader (station_id, card_reader_type, host, port)  
	VALUES 	(:station_id, :card_reader_type, :host, :port)
	ON CONFLICT (station_id)
	DO
	UPDATE SET card_reader_type = :card_reader_type, host = :host, port = :port, mtime = NOW()
	`
	sqlGetCardReaderConfig = `
	SELECT station_id, card_reader_type, host, port
	FROM card_reader
	WHERE station_id = :station_id
	`
	sqlUpdateConfigAdd = `
	INSERT INTO update_config (ctime, note)  
	VALUES 	(:ctime, :note)
	RETURNING 	id
	`
	sqlLastUpdateConfigGet = `
	SELECT max(id) as id from update_config
	`
	sqlAddAdvertisingCampaign = `
		INSERT INTO advertising_campaign (
			default_discount,
			discount_programs,
			end_date,
			end_minute,
			start_date,
			start_minute,
			weekday,
			enabled,
			name
		) VALUES (
			:default_discount,
			:discount_programs,
			:end_date,
			:end_minute,
			:start_date,
			:start_minute,
			:weekday,
			:enabled,
			:name
		) RETURNING *
	`

	sqlEditAdvertisingCampaign = `
		UPDATE advertising_campaign SET
			default_discount = :default_discount,
			discount_programs = :discount_programs,
			end_date = :end_date,
			end_minute = :end_minute,
			start_date = :start_date,
			start_minute = :start_minute,
			weekday = :weekday,
			enabled = :enabled,
			name = :name,
			version = version + 1,
			management_sended = false
		WHERE id = :id AND NOT deleted
	`

	sqlDelAdvertisingCampaign = `
		UPDATE advertising_campaign SET deleted = true, version = version + 1, management_sended = false
		WHERE id = :id AND NOT deleted
	`

	sqlAdvertisingCampaignByID = `
		SELECT 
			id,
			default_discount,
			discount_programs,
			end_date,
			end_minute,
			start_date,
			start_minute,
			weekday,
			enabled,
			name,
			version
		FROM advertising_campaign
		WHERE id = :id AND NOT deleted
	`

	sqlAdvertisingCampaign = `
		SELECT 
			id,
			default_discount,
			discount_programs,
			end_date,
			end_minute,
			start_date,
			start_minute,
			weekday,
			enabled,
			name
		FROM advertising_campaign
		WHERE (COALESCE(:start_date, end_date) <= end_date)
		  AND (COALESCE(:end_date, start_date) >= start_date)
		  AND NOT deleted
	`

	sqlCurrentAdvertisingCampaign = `
		SELECT
			id,
			default_discount,
			discount_programs,
			end_date,
			end_minute,
			start_date,
			start_minute,
			weekday,
			enabled,
			name
		FROM advertising_campaign
		WHERE enabled  
			AND :current_date BETWEEN start_date AND end_date
			AND NOT deleted
	`

	sqlGetConfigInt = `
	SELECT name, value, description, note
	FROM config_vars_int
	WHERE name = UPPER(:name)
	`
	sqlGetConfigBool = `
	SELECT name, value, description, note
	FROM config_vars_bool
	WHERE name = UPPER(:name)
	`
	sqlGetConfigString = `
	SELECT name, value, description, note, deleted, version
	FROM config_vars_string
	WHERE name = UPPER(:name)
	`

	sqlSetConfigInt = `
	INSERT INTO config_vars_int (name, value, description, note)
		VALUES (UPPER(:name), :value, :description, :note)
	ON CONFLICT (name)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = config_vars_int.version + 1, 
			management_sended = false
	`
	sqlSetConfigIntIfNotExists = `
	INSERT INTO config_vars_int (name, value, description, note)
		VALUES (UPPER(:name), :value, :description, :note)
	ON CONFLICT (name)
	DO NOTHING
	`
	sqlSetConfigBool = `
	INSERT INTO config_vars_bool (name, value, description, note)
		VALUES (UPPER(:name), :value, :description, :note)
	ON CONFLICT (name)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = config_vars_bool.version + 1, 
			management_sended = false
	`
	sqlSetConfigString = `
	INSERT INTO config_vars_string (name, value, description, note)
		VALUES (UPPER(:name), :value, :description, :note)
	ON CONFLICT (name)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = config_vars_string.version + 1, 
			management_sended = false
	`
	sqlDeleteConfigString = `
	UPDATE config_vars_string SET deleted = true, management_sended = false, version = version + 1 WHERE name = UPPER(:name)
	`

	sqlGetStationConfigInt = `
	SELECT name, value, description, note, station_id, version
	FROM station_config_vars_int
	WHERE name = UPPER(:name) and station_id = :station_id
	`
	sqlGetStationConfigBool = `
	SELECT name, value, description, note, station_id, version
	FROM station_config_vars_bool
	WHERE name = UPPER(:name) and station_id = :station_id
	`
	sqlGetStationConfigString = `
	SELECT name, value, description, note, station_id, version
	FROM station_config_vars_string
	WHERE name = UPPER(:name) and station_id = :station_id
	`

	sqlSetStationConfigInt = `
	INSERT INTO station_config_vars_int (name, value, description, note, station_id)
		VALUES (UPPER(:name), :value, :description, :note, :station_id)
	ON CONFLICT (name, station_id)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = station_config_vars_int.version + 1, 
			management_sended = false
	`
	sqlSetStationConfigBool = `
	INSERT INTO station_config_vars_bool (name, value, description, note, station_id)
		VALUES (UPPER(:name), :value, :description, :note, :station_id)
	ON CONFLICT (name, station_id)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = station_config_vars_bool.version + 1, 
			management_sended = false
	`
	sqlSetStationConfigString = `
	INSERT INTO station_config_vars_string (name, value, description, note, station_id)
		VALUES (UPPER(:name), :value, :description, :note, :station_id)
	ON CONFLICT (name, station_id)
	DO
		UPDATE 
			SET value = :value,
			description = :description,
			note = :note,
			version = station_config_vars_string.version + 1, 
			management_sended = false
	`

	sqlGetUnsendedRabbitMessages = `
	select id, message_type, payload, created_at, sent, sent_at 
	from bonus_rabbit_send_log 
	where sent = false AND id > :last_id
	order by id
	limit 100
	`

	sqlMarkRabbitMessageAsSent = `
	UPDATE bonus_rabbit_send_log SET sent = TRUE, sent_at = :ctime WHERE id = :id
	`

	sqlAddRabbitMessage = `
	INSERT INTO bonus_rabbit_send_log (message_type,payload,created_at) VALUES (:message_type,:payload,:created_at)
	`

	sqlAddRabbitMoneyReport = `
	INSERT INTO bonus_rabbit_money_report_send_log (message_type,money_report_id,created_at, message_uuid) VALUES (:message_type,:money_report_id,:created_at,:message_uuid)
	`

	sqlGetUnsendedRabbitMoneyReports = `
		select rabbit.id,
		   rabbit.message_type,
		   rabbit.created_at,
		   rabbit.sent,
		   rabbit.sent_at,
		   report.station_id,
		   report.banknotes,
		   report.cars_total,
		   report.coins,
		   report.electronical,
		   report.service,
		   report.bonuses,
		   report.session_id,
		   report.qr_money,
		   rabbit.message_uuid
		from bonus_rabbit_money_report_send_log rabbit
			LEFT JOIN money_report report on rabbit.money_report_id = report.id
		where rabbit.sent = false AND rabbit.id > :last_id
		order by rabbit.id
		limit 100
	`

	sqlAddMoneyReportForRabbitMessage = `
INSERT INTO money_report (station_id, banknotes, cars_total, coins, electronical, service, bonuses, ctime, session_id,qr_money)  
VALUES 	(:station_id, :banknotes, :cars_total, :coins, :electronical, :service, :bonuses, :ctime, :session_id,:qr_money)
returning id
	`

	sqlMarkRabbitMoneyReportAsSent = `
	UPDATE bonus_rabbit_money_report_send_log SET sent = TRUE, sent_at = :ctime WHERE id = :id
	`
	sqlRefreshMotorStatsCurrent   = `refresh materialized view mv_current_relay_stat`
	sqlRefreshProgramStatsCurrent = `refresh materialized view mv_current_program_stat`
	sqlRefreshMotorStatsDates     = `refresh materialized view mv_relay_stat_dates`
	sqlRefreshProgramStatsDates   = `refresh materialized view mv_program_stat_dates`

	sqlGetListBuildScripts = `
	SELECT
		id,
		station_id,
		name,
		commands
    FROM build_scripts
	ORDER BY station_id ASC
	`

	sqlGetBuildScript = `
	SELECT
		id,
		station_id,
		name,
		commands
    FROM build_scripts
	WHERE id = :id
	`

	sqlGetBuildScriptByStationID = `
	SELECT
		id,
		station_id,
		name,
		commands
    FROM build_scripts
	WHERE station_id = :id
	`

	sqlInsertBuildScript = `
	INSERT INTO build_scripts (station_id, name, commands)
	VALUES (:station_id, :name, :commands)
	RETURNING
		id,
		station_id,
		name,
		commands
	`

	sqlUpdateBuildScript = `
	UPDATE build_scripts
	SET
		station_id = :station_id, 
		name = :name, 
		commands = :commands
	WHERE id = :id
	RETURNING
		id,
		station_id,
		name,
		commands
	`

	sqlDeleteBuildScript = `
	DELETE FROM build_scripts
	WHERE id = :id
	`

	sqlDeleteBuildScriptByStationID = `
	DELETE FROM build_scripts
	WHERE station_id = :id
	`

	sqlGetTask = `
	SELECT
		id,
		station_id,
		version_id,
		type,
		status,
		retry_count,
		error,
		created_at,
		started_at,
		stopped_at
    FROM tasks
	WHERE id = :id
	`

	sqlGetListTasks = `
	SELECT
		id,
		station_id,
		version_id,
		type,
		status,
		retry_count,
		error,
		created_at,
		started_at,
		stopped_at,
		COUNT(id) OVER() AS total_count
    FROM tasks
	WHERE
		(CAST(:stations_id AS INT[]) 		      IS NULL OR station_id = ANY(:stations_id)) AND
		(CAST(:statuses    AS TASK_STATUS_ENUM[]) IS NULL OR status     = ANY(:statuses))    AND
		(CAST(:types       AS TASK_TYPE_ENUM[])   IS NULL OR type       = ANY(:types))
	ORDER BY
		CASE WHEN :sort = 'created_at_asc'  THEN created_at END ASC,
		CASE WHEN :sort = 'created_at_desc' THEN created_at END DESC,
		CASE WHEN CAST(:sort AS TEXT) IS NULL OR :sort not in ('created_at_asc', 'created_at_desc') THEN created_at END DESC
	LIMIT  CASE WHEN :limit  >= 1 THEN :limit  ELSE 10 END
	OFFSET CASE WHEN :offset >= 0 THEN :offset ELSE 0  END
	`
	sqlInsertTask = `
	INSERT INTO tasks (station_id, version_id, type)
	VALUES (:station_id, :version_id, :type)
	RETURNING
		id,
		station_id,
		version_id,
		type,
		status,
		retry_count,
		error,
		created_at,
		started_at,
		stopped_at
	`

	sqlUpdateTask = `
	UPDATE tasks
	SET
		status = COALESCE(:status, status),
		retry_count = COALESCE(:retry_count, retry_count),
		error = COALESCE(:error, error),
		started_at = COALESCE(:started_at, started_at),
		stopped_at = COALESCE(:stopped_at, stopped_at)
	WHERE id = :id
	RETURNING
		id,
		station_id,
		version_id,
		type,
		status,
		retry_count,
		error,
		created_at,
		started_at,
		stopped_at
	`

	sqlDeleteTask = `
	DELETE FROM tasks
	WHERE id = :id
	`

	sqlInsertOpenwashingLog = `
	INSERT INTO openwashing_logs (station_id, text, type, level, created_at)
	VALUES (:station_id, :text, :type, :level, :created_at)
	RETURNING
		id,
		station_id,
		text,
		type,
		level,
		created_at
	`
)

type (
	argUpdateConfigAdd struct {
		Ctime time.Time
		Note  string
	}
	argLastUpdateConfigGet struct {
	}
	resLastUpdateConfigGet struct {
		ID int
	}
	argSetCardReaderConfig struct {
		StationID      app.StationID
		CardReaderType string
		Host           string
		Port           string
	}
	argGetCardReaderConfig struct {
		StationID app.StationID
	}
	resGetCardReaderConfig struct {
		StationID      app.StationID
		CardReaderType string
		Host           string
		Port           string
	}
	argSetValue struct {
		StationID app.StationID
		Key       string
		Value     string
	}
	argGetValue struct {
		StationID app.StationID
		Key       string
	}
	argAddStation struct {
		Name string
	}
	argCheckDB struct {
	}
	argUpdStation struct {
		ID           app.StationID
		Name         string
		PreflightSec int
		RelayBoard   string
	}
	argStationHash struct {
		Hash      string
		StationID app.StationID
	}
	argDelStation struct {
		ID app.StationID
	}
	argGetStations struct {
	}
	argGetStation struct {
		ID app.StationID
	}
	argGetUser struct {
		Login string
	}
	argGetUsers struct {
	}
	argGetUserRoles struct {
		ID int
	}
	argAddUser struct {
		Login      string
		FirstName  string
		MiddleName string
		LastName   string
		Password   string
		IsAdmin    bool
		IsEngineer bool
		IsOperator bool
	}
	argUpdateUser struct {
		Login      string
		FirstName  string
		MiddleName string
		LastName   string
		Password   string
		IsAdmin    bool
		IsEngineer bool
		IsOperator bool
	}
	argUpdateUserPassword struct {
		Login       string
		NewPassword string
	}
	argDelUser struct {
		Login string
	}
	resUser struct {
		ID         int
		Login      string
		FirstName  string
		MiddleName string
		LastName   string
		Password   string
		IsAdmin    bool
		IsOperator bool
		IsEngineer bool
	}
	resGetValue struct {
		Value string
	}
	resStation struct {
		ID           app.StationID
		Name         string
		PreflightSec int
		RelayBoard   string
		Hash         *string
	}
	argLastMoneyReport struct {
		StationID app.StationID
	}
	argLastCollectionReport struct {
		StationID app.StationID
	}
	argCollectionReports struct {
		StationID app.StationID
	}
	argCollectionReportsByDate struct {
		StationID app.StationID
		StartDate *time.Time
		EndDate   *time.Time
	}
	argStationReport struct {
		StationID *app.StationID
	}
	argDatesStationStat struct {
		StationID *app.StationID
		StartDate time.Time
		EndDate   time.Time
	}
	argAddRelayReport struct {
		StationID  app.StationID
		ProgramID  int
		PumpTimeOn int
		TimeOn     int
		Ctime      time.Time
	}
	argAddRelayStat struct {
		RelayReportID int
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	resRelayStats struct {
		StationID     app.StationID
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	argResetRelayStat struct {
		StationID app.StationID
		Ver       int
		Data      string
	}

	argOpenStationLogAdd struct {
		StationID app.StationID
	}
	resRelayReport struct {
		StationID   app.StationID
		ProgramID   int
		ProgramName string
		TimeOn      int
		PumpTimeOn  int
	}
	resRelayStat struct {
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	argRelayStat struct {
		RelayReportID app.StationID
	}
	argMoneyReport struct {
		StationID app.StationID
		StartDate time.Time
		EndDate   time.Time
	}
	argCurrentMoney struct {
		StationID app.StationID
	}
	argAddCollectionReport struct {
		StationID app.StationID
		UserID    int
		Ctime     time.Time
	}
	argRelayStatReport struct {
		StationID app.StationID
		StartDate time.Time
		EndDate   time.Time
	}
	resStationsVariables struct {
		ID    app.StationID
		Name  string
		Key   string
		Value string
	}
	resLoadHash struct {
		Hash      string
		StationID app.StationID
	}
	resCheckDB struct {
		CountColumns int
	}

	argPrograms struct {
		ID *int64
	}

	resProgram struct {
		ID                         int64
		Price                      int
		Name                       string
		Note                       string
		PreflightEnabled           bool
		Relays                     string
		PreflightRelays            string
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		IsFinishingProgram         bool
		Version                    int
		ManagementSended           bool
	}

	argStationProgram struct {
		StationID app.StationID
	}
	argStationProgramDel struct {
		StationID app.StationID
	}
	argStationProgramAdd struct {
		StationID app.StationID
		ProgramID int
		ButtonID  int
	}

	resStationProgram struct {
		StationID app.StationID
		ProgramID int
		ButtonID  int
	}
	argStationConfig struct {
		ID app.StationID
	}
	resStationConfig struct {
		ID                         app.StationID
		Price                      int
		Name                       string
		PreflightSec               int
		ProgramID                  int64
		ButtonID                   int
		ProgramName                string
		PreflightEnabled           bool
		IsFinishingProgram         bool
		Relays                     string
		PreflightRelays            string
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		RelayBoard                 string
	}

	resCollectionReportByDate struct {
		StationID    app.StationID
		Banknotes    int
		CarsTotal    int
		Coins        int
		Electronical int
		Service      int
		Bonuses      int
		QrMoney      int
		Ctime        time.Time
		User         string
	}

	argSetProgram struct {
		ID                         int64
		Price                      int
		Name                       string
		PreflightEnabled           bool
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		Relays                     string
		PreflightRelays            string
		IsFinishingProgram         bool
	}

	argKasseGet struct {
	}

	resKasse struct {
		ReceiptItem     string
		TaxType         string
		CashierFullName string
		CashierINN      string
	}
	argSetKasse struct {
		ReceiptItem     string
		TaxType         string
		CashierFullName string
		CashierINN      string
	}
	argAddMoneyReport struct {
		StationID    app.StationID
		Banknotes    int
		CarsTotal    int
		Coins        int
		Electronical int
		Service      int
		Bonuses      int
		Ctime        time.Time
		SessionID    string
		QrMoney      int
	}
	argAdvertisingCampaign struct {
		DefaultDiscount  int64
		DiscountPrograms string
		EndDate          time.Time
		EndMinute        int64
		ID               int64
		StartDate        time.Time
		StartMinute      int64
		Weekday          string
		Enabled          bool
		Name             string
	}
	resAdvertisingCampaign struct {
		ID               int64
		DefaultDiscount  int64
		DiscountPrograms string
		EndDate          time.Time
		EndMinute        int64
		StartDate        time.Time
		StartMinute      int64
		Weekday          string
		Enabled          bool
		Name             string
		Ctime            time.Time
		Deleted          bool
		Version          int
		ManagementSended bool
	}
	argDelAdvertisingCampaign struct {
		ID int64
	}
	argAdvertisingCampaignByID struct {
		ID int64
	}
	argAdvertisingCampaignGet struct {
		StartDate *time.Time
		EndDate   *time.Time
	}

	argCurrentAdvertisingCampagins struct {
		CurrentDate time.Time
	}

	argGetConfig struct {
		Name string
	}
	argGetStationConfig struct {
		Name      string
		StationID app.StationID
	}
	argSetConfigInt struct {
		Name        string
		Value       int64
		Description string
		Note        string
	}
	argSetConfigBool struct {
		Name        string
		Value       bool
		Description string
		Note        string
	}
	argSetConfigString struct {
		Name        string
		Value       string
		Description string
		Note        string
	}

	argGetUnsendedRabbitMessages struct {
		LastID int64
	}

	argAddRabbitMoneyReport struct {
		MessageType   string
		MoneyReportID int
		CreatedAt     time.Time
		MessageUUID   uuid.NullUUID
	}

	argAddRabbitMessage struct {
		MessageType string
		Payload     []byte
		CreatedAt   time.Time
	}

	argMarkRabbitMessageAsSent struct {
		ID    int64
		Ctime time.Time
	}

	resGetConfigInt struct {
		Name        string
		Value       int64
		Description string
		Note        string
		Version     int
	}
	resGetConfigBool struct {
		Name        string
		Value       bool
		Description string
		Note        string
		Version     int
	}
	resGetConfigString struct {
		Name        string
		Value       string
		Description string
		Note        string
		Deleted     bool
		Version     int
	}

	argSetStationConfigVar[T comparable] struct {
		Name        string
		Value       T
		Description string
		Note        string
		StationID   app.StationID
	}
	resGetStationConfigVar[T comparable] struct {
		Name        string
		Value       T
		Description string
		Note        string
		StationID   app.StationID
		Version     int
	}

	resRabbitMessage struct {
		ID          int64
		MessageType string
		Payload     []byte
		CreatedAt   time.Time
		Sent        bool
		SentAt      *time.Time
	}
	resRabbitMoneyReport struct {
		ID            int64
		MessageType   string
		MoneyReportID int
		StationID     int
		Banknotes     int
		CarsTotal     int
		Coins         int
		Electronical  int
		Service       int
		Bonuses       int
		SessionID     string
		QrMoney       int
		CreatedAt     time.Time
		Sent          bool
		SentAt        *time.Time
		MessageUUID   uuid.NullUUID
	}

	resBuildScript struct {
		ID        int
		StationID int
		Name      string
		Commands  string
	}

	argInsertBuildScript struct {
		StationID int
		Name      string
		Commands  string
	}

	argUpdateBuildScript struct {
		ID        int
		StationID int
		Name      string
		Commands  string
	}

	argGetBuildScript struct {
		ID int
	}

	TaskType   string
	TaskStatus string
	TaskSort   string
	LogLevel   string

	resTask struct {
		ID         int
		StationID  int
		VersionID  *int
		Type       TaskType
		Status     TaskStatus
		RetryCount int
		Error      *string
		CreatedAt  time.Time
		StartedAt  *time.Time
		StoppedAt  *time.Time
	}

	resTasks struct {
		ID         int
		StationID  int
		VersionID  *int
		Type       TaskType
		Status     TaskStatus
		RetryCount int
		Error      *string
		CreatedAt  time.Time
		StartedAt  *time.Time
		StoppedAt  *time.Time
		TotalCount int
	}

	argGetTask struct {
		ID int
	}

	argGetListTasks struct {
		StationsID pq.Int32Array
		Statuses   pq.StringArray
		Types      pq.StringArray
		Sort       *TaskSort
		Limit      int
		Offset     int
	}

	argInsertTask struct {
		StationID int
		VersionID *int
		Type      TaskType
	}

	argUpdateTask struct {
		ID         int
		Status     *TaskStatus
		RetryCount *int
		Error      *string
		StartedAt  *time.Time
		StoppedAt  *time.Time
	}

	argInsertOpenwashingLog struct {
		StationID int
		Text      string
		Type      *string
		Level     LogLevel
		CreatedAt time.Time
	}

	respOpenwashingLog struct {
		ID        int64
		StationID int
		Text      string
		Type      *string
		Level     LogLevel
		CreatedAt time.Time
	}
)

const (
	BuildTaskType        TaskType = "build"
	UpdateTaskType       TaskType = "update"
	RebootTaskType       TaskType = "reboot"
	GetVersionsTaskType  TaskType = "get_versions"
	PullFirmwareTaskType TaskType = "pull_firmware"
	SetVersionTaskType   TaskType = "set_version"

	QueueTaskStatus     TaskStatus = "queue"
	StartedTaskStatus   TaskStatus = "started"
	CompletedTaskStatus TaskStatus = "completed"
	ErrorTaskStatus     TaskStatus = "error"
	CanceledTaskStatus  TaskStatus = "canceled"

	CreatedAtAscTaskSort  TaskSort = "created_at_asc"
	CreatedAtDescTaskSort TaskSort = "created_at_desc"

	DebugLogLevel   LogLevel = "debug"
	InfoLogLevel    LogLevel = "info"
	WarningLogLevel LogLevel = "warning"
	ErrorLogLevel   LogLevel = "error"
)
