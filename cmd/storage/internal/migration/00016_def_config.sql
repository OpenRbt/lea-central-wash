-- +goose Up
-- SQL in this section is executed when the migration is applied.

INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (1, 'station1', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (2, 'station2', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (3, 'station3', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (4, 'station4', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (5, 'station5', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (6, 'station6', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (7, 'station7', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (8, 'station8', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (9, 'station9', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (10, 'station10', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (11, 'station11', 'localGPIO', 0)  ON CONFLICT DO NOTHING;
INSERT INTO station (id, name, relay_board, preflight_sec) VALUES (12, 'station12', 'localGPIO', 0)  ON CONFLICT DO NOTHING;

INSERT INTO program (id, name, price, relays) VALUES (1, 'water', 25, '[{"ID":1,"TimeOn":1000,"TimeOff":0},{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;
INSERT INTO program (id, name, price, relays) VALUES (2, 'foam', 25, '[{"ID":1,"TimeOn":1000,"TimeOff":0},{"ID":2,"TimeOn":278,"TimeOff":722},{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;
INSERT INTO program (id, name, price, relays) VALUES (3, 'foam active', 25, '[{"ID":1,"TimeOn":1000,"TimeOff":0},{"ID":2,"TimeOn":500,"TimeOff":500},{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;
INSERT INTO program (id, name, price, relays) VALUES (4, 'wax', 25, '[{"ID":1,"TimeOn":1000,"TimeOff":0},{"ID":4,"TimeOn":284,"TimeOff":716},{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;
INSERT INTO program (id, name, price, relays) VALUES (5, 'osmosian', 25, '[{"ID":5,"TimeOn":1000,"TimeOff":0},{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;
INSERT INTO program (id, name, price, relays) VALUES (6, 'pause', 25, '[{"ID":6,"TimeOn":1000,"TimeOff":0}]') ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (1,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (2,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (3,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (4,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (5,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (6,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (7,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (8,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (9,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (10,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (11,6,6) ON CONFLICT DO NOTHING;

INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,1,1) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,2,2) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,3,3) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,4,4) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,5,5) ON CONFLICT DO NOTHING;
INSERT INTO station_program (station_id, program_id, button_id) VALUES (12,6,6) ON CONFLICT DO NOTHING;

INSERT INTO kasse(receipt_item, tax_type, cashier_full_name, cashier_inn) VALUES ('мойка автомобиля','TAX_NO', '', '');

INSERT INTO card_reader(station_id, card_reader_type) VALUES (1,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (2,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (3,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (4,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (5,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (6,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (7,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (8,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (9,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (10,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (11,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;
INSERT INTO card_reader(station_id, card_reader_type) VALUES (12,'PAYMENT_WORLD') ON CONFLICT DO NOTHING;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

TRUNCATE TABLE station_program;
TRUNCATE TABLE program CASCADE;
