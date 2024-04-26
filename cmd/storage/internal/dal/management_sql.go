package dal

import "github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"

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

	sqlStationNotSended = `
		SELECT id, name, preflight_sec, relay_board, deleted, version FROM station where not management_sended
			`

	sqlSetStation = `
		INSERT INTO station (id, name, preflight_sec, relay_board, deleted, version, management_sended)  
		VALUES 	(:id, :name, :preflight_sec, :relay_board, :deleted, :version, true)
		ON CONFLICT (id) DO
		UPDATE 
		SET name = :name, preflight_sec = :preflight_sec, relay_board = :relay_board, deleted = :deleted, version = :version, management_sended = true
		WHERE ((station.version<:version) or :force)
				`
	sqlSetStationSensed = `
		UPDATE station
		SET management_sended = true
		WHERE id = :id
		`
	sqlSetStationHashSensed = `
		UPDATE station_hash
		SET management_sended = true
		WHERE station_id = :id
			`
	sqlStationHashNotSended = `
		SELECT station_id, hash FROM station_hash where not management_sended
				`
)

type (
	argSetStation struct {
		ID           app.StationID
		Name         string
		PreflightSec int
		RelayBoard   string
		Version      int
		Deleted      bool
		Force        bool
	}
	argID struct {
		ID int
	}

	rowStation struct {
		ID           app.StationID
		Name         string
		PreflightSec int
		RelayBoard   string
		Version      int
		Deleted      bool
	}
)
