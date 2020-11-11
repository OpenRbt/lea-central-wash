package dal

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
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
SET name = :name
WHERE id = :id
	`
	sqlGetStation = `
SELECT id, name  FROM station where deleted = false ORDER BY id
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
INSERT INTO money_report (station_id, banknotes, cars_total, coins, electronical, service)  
VALUES 	(:station_id, :banknotes, :cars_total, :coins, :electronical, :service)
	`
	sqlAddCollectionReport = `
INSERT INTO money_collection (station_id, money)  
VALUES 	(:station_id, :money)
	`
	sqlLastMoneyReport = `
SELECT station_id, banknotes, cars_total, coins, electronical, service FROM money_report WHERE station_id = :station_id
ORDER BY id DESC
LIMIT 1
	`
	sqlLastCollectionReport = `
SELECT station_id, money, ctime FROM money_collection WHERE station_id = :station_id
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
		FROM money_report WHERE station_id = :station_id and ctime <= :start_date
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
)

type (
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
		ID   app.StationID
		Name string
	}
	argStationHash struct {
		Hash      string
		StationID app.StationID
	}
	argDelStation struct {
		ID app.StationID
	}
	argGetStation struct {
	}
	resGetValue struct {
		Value string
	}
	resStation struct {
		ID   app.StationID
		Name string
	}
	argLastMoneyReport struct {
		StationID app.StationID
	}
	argLastCollectionReport struct {
		StationID app.StationID
	}
	argLastRelayReport struct {
		StationID app.StationID
	}
	argAddRelayReport struct {
		StationID app.StationID
	}
	argAddRelayStat struct {
		RelayReportID int
		RelayID       int
		SwitchedCount int
		TotalTimeOn   int64
	}
	argOpenStationLogAdd struct {
		StationID app.StationID
	}
	resRelayReport struct {
		ID app.StationID
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
)
