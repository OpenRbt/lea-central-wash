package dal

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

const (
	constraintCardReaderStationID  = "card_reader_station_id_fkey"
	constraintUserLogin            = "users_unique_lower_login_idx"
	constraintMoneyCollection      = "money_collection_user_id_fkey"
	constraintStationProgramID     = "station_program_program_id_fkey"
	constraintStationStationID     = "station_program_station_id_fkey"
	constraintStationProgramUnique = "station_program_pkey"
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
SELECT id, name, preflight_sec, relay_board FROM station where deleted = false ORDER BY id
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
INSERT INTO money_report (station_id, banknotes, cars_total, coins, electronical, service, ctime)  
VALUES 	(:station_id, :banknotes, :cars_total, :coins, :electronical, :service, :ctime)
	`
	sqlAddCollectionReport = `
	INSERT INTO money_collection (station_id, user_id, banknotes, cars_total, coins, electronical, service, last_money_report_id, ctime) 
	(
	SELECT station_id, 
			   :user_id,
			   sum(banknotes) as banknotes, 
			   sum(cars_total) as cars_total, 
			   sum(coins) as coins, 
			   sum(electronical) as electronical, 
			   sum(service) as service,
			   max(id) as max_id,
			   :ctime
		FROM money_report
	WHERE station_id = :station_id and id > coalesce(
	(SELECT last_money_report_id FROM money_collection WHERE station_id = :station_id
	ORDER BY id DESC
	LIMIT 1)
	,0)
	GROUP BY station_id
	)	
	`
	sqlLastMoneyReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service FROM money_report WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlLastCollectionReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service, ctime FROM money_collection WHERE station_id = :station_id
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
		mc.ctime,
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
SELECT r.station_id, r.program_id, coalesce(p.name, '') as program_name, sum(r.time_on) as time_on ,sum(r.pump_time_on) as pump_time_on FROM relay_report r
LEFT JOIN program p on p.id = r.program_id
WHERE r.id > coalesce(
	(SELECT last_relay_report_id FROM reset_relay_report WHERE station_id = r.station_id
	ORDER BY id DESC
	LIMIT 1)
	,0)
AND (r.station_id = :station_id or CAST(:station_id AS INT) is NULL)
GROUP BY r.station_id,r.program_id,coalesce(p.name, '')
ORDER BY r.station_id,r.program_id
`
	sqlCurentRelayStat = `
	SELECT p.station_id, r.relay_id, sum(r.switched_count) as switched_count, sum(r.total_time_on) as total_time_on
	FROM relay_stat r
	JOIN relay_report p on r.relay_report_id = p.id
	WHERE p.id > coalesce(
		(SELECT last_relay_report_id FROM reset_relay_report WHERE station_id = p.station_id
		ORDER BY id DESC
		LIMIT 1)
		,0)
	AND (station_id = :station_id or CAST(:station_id AS INT) is NULL)
	Group BY p.station_id, r.relay_id
	ORDER BY p.station_id, r.relay_id
	`
	sqlDatesStationReport = `
SELECT r.station_id, r.program_id, coalesce(p.name, '') as program_name, sum(r.time_on) as time_on ,sum(r.pump_time_on) as pump_time_on FROM relay_report r
LEFT JOIN program p on p.id = r.program_id
WHERE r.ctime >= :start_date AND r.ctime <= :end_date
AND (r.station_id = :station_id or CAST(:station_id AS INT) is NULL)
GROUP BY r.station_id,r.program_id,coalesce(p.name, '')
ORDER BY r.station_id,r.program_id
`
	sqlDatesRelayStat = `
	SELECT p.station_id, r.relay_id, sum(r.switched_count) as switched_count, sum(r.total_time_on) as total_time_on
	FROM relay_stat r
	JOIN relay_report p on r.relay_report_id = p.id
	WHERE p.ctime >= :start_date AND p.ctime <= :end_date
	AND (station_id = :station_id or CAST(:station_id AS INT) is NULL)
	Group BY p.station_id, r.relay_id
	ORDER BY p.station_id, r.relay_id
	`

	sqlMoneyReport = `
	SELECT station_id, 
		   sum(banknotes) as banknotes, 
		   sum(cars_total) as cars_total, 
		   sum(coins) as coins, 
		   sum(electronical) as electronical, 
		   sum(service) as service 
	FROM money_report
	WHERE :start_date < ctime AND ctime <= :end_date AND station_id = :station_id
	GROUP BY station_id
	`
	sqlCurrentMoney = `
	SELECT station_id, 
		   sum(banknotes) as banknotes, 
		   sum(cars_total) as cars_total, 
		   sum(coins) as coins, 
		   sum(electronical) as electronical, 
		   sum(service) as service 
	FROM money_report
	WHERE id > coalesce(
		(SELECT last_money_report_id FROM money_collection WHERE station_id = :station_id
		ORDER BY id DESC
		LIMIT 1)
		,0)
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
	is_finishing_program
    FROM program
	WHERE ((id = :id) or (CAST(:id as integer) is null)) 
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
		)
	VALUES (
		:id,
		:price,
		:name,
		:preflight_enabled,
		:relays,
		:preflight_relays,
		:motor_speed_percent,
		:preflight_motor_speed_percent,
		:is_finishing_program
		) ON CONFLICT (id) DO
	UPDATE
	SET
	price = :price,
	name = :name,
	preflight_enabled = :preflight_enabled,
	relays = :relays,
	preflight_relays = :preflight_relays,
	motor_speed_percent = :motor_speed_percent,
	preflight_motor_speed_percent = :preflight_motor_speed_percent,
	is_finishing_program = :is_finishing_program
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
		timezone,
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
		:timezone,
		:weekday,
		:enabled,
		:name
	)
	`
	sqlEditAdvertisingCampaign = `
	UPDATE advertising_campaign SET
		default_discount = :default_discount,
		discount_programs = :discount_programs,
		end_date = :end_date,
		end_minute = :end_minute,
		start_date = :start_date,
		start_minute = :start_minute,
		timezone = :timezone,
		weekday = :weekday,
		enabled = :enabled,
		name = :name
	WHERE id = :id
		`
	sqlDelAdvertisingCampaign = `
		DELETE FROM advertising_campaign
		WHERE id = :id
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
		timezone,
		weekday,
		enabled,
		name
	FROM advertising_campaign
	WHERE id = :id
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
	timezone,
	weekday,
	enabled,
	name
FROM advertising_campaign
WHERE (:start_date <= end_date or CAST(:start_date AS TIMESTAMP) is null) AND (:end_date >= start_date or CAST(:start_date AS TIMESTAMP) is null)
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

	resPrograms struct {
		ID                         int64
		Price                      int
		Name                       string
		PreflightEnabled           bool
		Relays                     string
		PreflightRelays            string
		MotorSpeedPercent          int64
		PreflightMotorSpeedPercent int64
		IsFinishingProgram         bool
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
		Ctime        time.Time
	}
	argAdvertisingCampaign struct {
		DefaultDiscount  int64
		DiscountPrograms string
		EndDate          time.Time
		EndMinute        int64
		ID               int64
		StartDate        time.Time
		StartMinute      int64
		Timezone         int64
		Weekday          string
		Enabled          bool
		Name             string
	}
	resAdvertisingCampaign struct {
		DefaultDiscount  int64
		DiscountPrograms string
		EndDate          time.Time
		EndMinute        int64
		ID               int64
		StartDate        time.Time
		StartMinute      int64
		Timezone         int64
		Weekday          string
		Enabled          bool
		Name             string
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
)
