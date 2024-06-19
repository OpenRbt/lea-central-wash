package dal

import (
	"time"
)

const (
	sqlCollections = `
		SELECT id, station_id, banknotes, cars_total, coins, electronical, service, ctime, bonuses,qr_money, management_id FROM money_collection where not management_sended
		LIMIT 10
	`
	sqlCollectionSetSended = `
		UPDATE money_collection SET management_sended = true WHERE id = :id
	`
	sqlMoneySetSended = `
		UPDATE money_report SET management_sended = true WHERE id = :id
	`
	sqlMoney = `
		SELECT r.id, r.station_id, r.banknotes, r.cars_total, r.coins, r.electronical, r.service, r.bonuses, r.qr_money, r.management_message_id, r.ctime
		, c.management_id as collection_report_id, c.last_money_report_id
		FROM money_report as r
		JOIN
		(
		SELECT t.station_id, t.management_id, t.last_money_report_id, t.management_sended FROM money_collection t
		JOIN (SELECT station_id, max(id) as "id" FROM money_collection GROUP BY station_id) m on t.id = m.id and t.station_id = m.station_id
		) c on r.station_id = c.station_id
		where not r.management_sended and c.management_sended
		LIMIT 10
	`
	sqlNotSendedAdvertisingCampaigns = `
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
			version,
			deleted
		FROM advertising_campaign 
		WHERE NOT management_sended
	`
	sqlUpsertAdvertisingCampaignFromManagement = `
		INSERT INTO advertising_campaign (
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
		) VALUES (
			:id,
			:default_discount,
			:discount_programs,
			:end_date,
			:end_minute,
			:start_date,
			:start_minute,
			:weekday,
			:enabled,
			:name,
			:version
		) ON CONFLICT (id) DO UPDATE SET
			default_discount  = EXCLUDED.default_discount,
			discount_programs = EXCLUDED.discount_programs,
			end_date 		  = EXCLUDED.end_date,
			end_minute 		  = EXCLUDED.end_minute,
			start_date 		  = EXCLUDED.start_date,
			start_minute 	  = EXCLUDED.start_minute,
			weekday 		  = EXCLUDED.weekday,
			enabled 		  = EXCLUDED.enabled,
			name 			  = EXCLUDED.name,
			version 		  = EXCLUDED.version,
			management_sended = false
		WHERE :force OR advertising_campaign.version < EXCLUDED.version
		RETURNING *
	`
	sqlMarkAdvertisingCampaignSended = `
		UPDATE advertising_campaign
		SET management_sended = true
		WHERE id = :id
	`
	sqlNotSendedPrograms = `
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
		WHERE NOT management_sended
	`
	sqlNotSendedOpenwashingLogs = `
		SELECT 
			id,
			station_id,
			text,
			type,
			level,
			created_at
		FROM openwashing_logs 
		WHERE NOT management_sended
		ORDER BY created_at
	`
	sqlMarkOpenwashingLogSended = `
		UPDATE openwashing_logs
		SET management_sended = true
		WHERE id = :id
	`
	sqlNotSendedConfigStrings = `
		SELECT 
			name,
			value,
			description,
			note,
			deleted,
			version
		FROM config_vars_string
		WHERE NOT management_sended
	`
	sqlMarkConfigStringSended = `
		UPDATE config_vars_string
		SET management_sended = true
		WHERE name = UPPER(:id)
	`
	sqlNotSendedConfigInts = `
		SELECT 
			name,
			value,
			description,
			note,
			version
		FROM config_vars_int
		WHERE NOT management_sended
	`
	sqlMarkConfigIntSended = `
		UPDATE config_vars_int
		SET management_sended = true
		WHERE name = UPPER(:id)
	`
	sqlNotSendedConfigBools = `
	SELECT 
		name,
		value,
		description,
		note,
		version
	FROM config_vars_bool
	WHERE NOT management_sended
	`
	sqlMarkConfigBoolSended = `
	UPDATE config_vars_bool
	SET management_sended = true
	WHERE name = UPPER(:id)
	`
	sqlNotSendedStationConfigBools = `
	SELECT 
		name,
		station_id,
		value,
		description,
		note,
		version
	FROM station_config_vars_bool
	WHERE NOT management_sended
	`
	sqlMarkStationConfigBoolSended = `
	UPDATE station_config_vars_bool
	SET management_sended = true
	WHERE name = UPPER(:name) and station_id = :station_id
	`
	sqlNotSendedStationConfigStrings = `
	SELECT 
		name,
		station_id,
		value,
		description,
		note,
		version
	FROM station_config_vars_string
	WHERE NOT management_sended
	`
	sqlMarkStationConfigStringSended = `
	UPDATE station_config_vars_string
	SET management_sended = true
	WHERE name = UPPER(:name) and station_id = :station_id
	`
	sqlNotSendedStationConfigInts = `
	SELECT 
		name,
		station_id,
		value,
		description,
		note,
		version
	FROM station_config_vars_int
	WHERE NOT management_sended
	`
	sqlMarkStationConfigIntSended = `
	UPDATE station_config_vars_int
	SET management_sended = true
	WHERE name = UPPER(:name) and station_id = :station_id
	`
	sqlNotSendedUsers = `
	SELECT 	id,
			login,
			first_name,
			middle_name,
			last_name,
			password,
			is_admin,
			is_operator,
			is_engineer,
			deleted,
			version
	FROM users
	WHERE NOT management_sended
	`
	sqlMarkTaskSended = `
	UPDATE tasks
	SET management_sended = true
	WHERE id = :id
	`
	sqlNotSendedTasks = `
	SELECT 	id,
			station_id,
			version_id,
			type,
			status,
			retry_count,
			error,
			created_at,
			started_at,
			stopped_at,
			version
	FROM tasks
	WHERE NOT management_sended
	`
	sqlMarkStationSended = `
	UPDATE station
	SET management_sended = true
	WHERE id = :id
	`
	sqlNotSendedStations = `
	select 	s.id,
			s.name,
			s.preflight_sec,
			s.relay_board,
			s.deleted,
			s.version,
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
	FROM station s
		join station_program b on s.id=b.station_id
		join program p on b.program_id=p.id
	WHERE NOT s.management_sended
	order by s.id, b.button_id
	`
	sqlMarkUserSended = `
	UPDATE users
	SET management_sended = true
	WHERE login = lower(:id)
	`
	sqlSetProgramFromManagement = `
		INSERT INTO program (
			id,
			price,
			name,
			preflight_enabled,
			relays,
			preflight_relays,
			motor_speed_percent,
			preflight_motor_speed_percent,
			is_finishing_program,
			version,
			management_sended
		) VALUES (
			:id,
			:price,
			:name,
			:preflight_enabled,
			:relays,
			:preflight_relays,
			:motor_speed_percent,
			:preflight_motor_speed_percent,
			:is_finishing_program,
			:version,
			true
		) ON CONFLICT (id) DO UPDATE SET 
			price 			    		  = EXCLUDED.price,
			name  			    		  = EXCLUDED.name,
			preflight_enabled   		  = EXCLUDED.preflight_enabled,
			relays              		  = EXCLUDED.relays,
			preflight_relays    		  = EXCLUDED.preflight_relays,
			motor_speed_percent 		  = EXCLUDED.motor_speed_percent,
			preflight_motor_speed_percent = EXCLUDED.preflight_motor_speed_percent,
			is_finishing_program  		  = EXCLUDED.is_finishing_program,
			version 		  	  		  = EXCLUDED.version,
			management_sended 	  		  = false
		WHERE :force OR program.version < EXCLUDED.version
		RETURNING *
	`
	sqlMarkProgramSended = `
		UPDATE program
		SET management_sended = true
		WHERE id = :id
	`
)

type (
	argID[T any] struct {
		ID T
	}

	argManagementProgram struct {
		ID                         int64
		Price                      int
		Name                       string
		PreflightEnabled           bool
		Relays                     string
		PreflightRelays            string
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		IsFinishingProgram         bool
		Version                    int
		Force                      bool
	}

	argManagementAdvertisingCampaign struct {
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
		Version          int
		Force            bool
	}
)
