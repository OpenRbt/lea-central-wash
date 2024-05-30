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
			default_discount  = :default_discount,
			discount_programs = :discount_programs,
			end_date 		  = :end_date,
			end_minute 		  = :end_minute,
			start_date 		  = :start_date,
			start_minute 	  = :start_minute,
			weekday 		  = :weekday,
			enabled 		  = :enabled,
			name 			  = :name,
			version 		  = CASE WHEN :force THEN :version ELSE advertising_campaign.version + 1 END,
			management_sended = false
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
			price = EXCLUDED.price,
			name = EXCLUDED.name,
			preflight_enabled = EXCLUDED.preflight_enabled,
			relays = EXCLUDED.relays,
			preflight_relays = EXCLUDED.preflight_relays,
			motor_speed_percent = EXCLUDED.motor_speed_percent,
			preflight_motor_speed_percent = EXCLUDED.preflight_motor_speed_percent,
			is_finishing_program = EXCLUDED.is_finishing_program,
			version = CASE WHEN :force THEN EXCLUDED.version ELSE program.version + 1 END,
			management_sended = true
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
