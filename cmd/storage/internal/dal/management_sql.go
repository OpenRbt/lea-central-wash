package dal

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
)
