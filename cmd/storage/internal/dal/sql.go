package dal

import "time"

const (
	sqlSetValue = `
INSERT INTO keypair (station_id, key, value)  
VALUES 	(:station_id, :key, :value)
ON CONFLICT (station_id, key)
DO
	UPDATE SET value = :value
	`
	sqlGetValue = `
SELECT value  FROM keypair  WHERE station_id = :station_id and key = :key
	`
	sqlAddStation = `
INSERT INTO station (hash, name)  
VALUES 	(:hash, :name)
	`
	sqlUpdStation = `
UPDATE station
SET hash = :hash, name = :name
WHERE id = :id
	`
	sqlGetStation = `
SELECT id, hash, name  FROM station where deleted = false ORDER BY id
	`
	sqlStationNullHash = `
UPDATE station
SET hash = null
WHERE hash = :hash
	`
	sqlDelStation = `
UPDATE station
SET deleted = true, hash = null
WHERE id = :id
	`
	sqlAddMoneyReport = `
INSERT INTO money_report (station_id, banknotes, cars_total, coins, electronical, service)  
VALUES 	(:station_id, :banknotes, :cars_total, :coins, :electronical, :service)
	`
	sqlLastMoneyReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service FROM money_report WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlAddRelayReport = `
INSERT INTO relay_report (station_id)  
VALUES 	(:station_id) RETURNING id
	`
	sqlAddRelayStat = `
INSERT INTO relay_stat (relay_report_id, relay_id, switched_count, total_time_on)
VALUES 	(:relay_report_id, :relay_id, :switched_count, :total_time_on)
	`
	sqlLastRelayReport = `
SELECT id FROM relay_report WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlRelayStat = `
SELECT relay_id, switched_count, total_time_on FROM relay_stat WHERE relay_report_id = :relay_report_id
ORDER BY relay_id
	`
	sqlMoneyReport = `
SELECT station_id, sum(banknotes) banknotes, sum(cars_total) cars_total, sum(coins) coins, sum(electronical) electronical, sum(service) service FROM (
		(SELECT station_id, banknotes, cars_total, coins, electronical, service 
		FROM money_report WHERE station_id = :station_id and ctime <= :end_date 
		ORDER BY id DESC
		LIMIT 1
		)
		union all
		(
		SELECT station_id, -banknotes, -cars_total, -coins, -electronical, -service 
		FROM money_report WHERE station_id = :station_id and ctime< :start_date
		ORDER BY id DESC
		LIMIT 1
		) 
		) AS MR
GROUP BY station_id
	`
	sqlRelayStatReport = `
	SELECT relay_id, sum(switched_count) switched_count, sum(total_time_on) total_time_on FROM (
		(SELECT relay_id, switched_count, total_time_on 
		FROM relay_stat WHERE relay_report_id = 
			(select id from relay_report where station_id = 1 and ctime <= :end_date
			 ORDER BY id DESC
			 LIMIT 1)
		)
		UNION ALL
		(SELECT relay_id, -switched_count, -total_time_on 
		FROM relay_stat WHERE relay_report_id = 
			(select id from relay_report where station_id = 1 and ctime< :start_date
			 ORDER BY id DESC
			 LIMIT 1)
		)
		) AS RR
		GROUP BY relay_id	
	`
)

type (
	argSetValue struct {
		StationID int
		Key       string
		Value     string
	}
	argGetValue struct {
		StationID int
		Key       string
	}
	argAddStation struct {
		Hash string
		Name string
	}
	argUpdStation struct {
		ID   int
		Hash string
		Name string
	}
	argStationNullHash struct {
		Hash string
	}
	argDelStation struct {
		ID int
	}
	argGetStation struct {
	}
	resGetValue struct {
		Value string
	}
	resStation struct {
		Hash *string
		ID   int
		Name string
	}
	argLastMoneyReport struct {
		StationID int
	}
	argLastRelayReport struct {
		StationID int
	}
	argAddRelayReport struct {
		StationID int
	}
	argAddRelayStat struct {
		RelayReportID int
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	resRelayReport struct {
		ID int
	}
	resRelayStat struct {
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	argRelayStat struct {
		RelayReportID int
	}
	argMoneyReport struct {
		StationID int
		StartDate time.Time
		EndDate   time.Time
	}
	argRelayStatReport struct {
		StationID int
		StartDate time.Time
		EndDate   time.Time
	}
)
