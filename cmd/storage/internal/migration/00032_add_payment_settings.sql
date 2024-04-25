-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE sbp_payments RENAME COLUMN confirmed TO authorized;
ALTER TABLE sbp_payments ADD COLUMN confirmed BOOLEAN DEFAULT false NOT NULL;
ALTER TABLE sbp_payments ADD COLUMN last_confirmed_at TIMESTAMP;
ALTER TABLE sbp_payments ADD COLUMN sent_confirmed BOOLEAN DEFAULT false NOT NULL;

UPDATE sbp_payments
SET confirmed = true
WHERE openwash_received;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE sbp_payments DROP COLUMN last_confirmed_at;
ALTER TABLE sbp_payments DROP COLUMN confirmed;
ALTER TABLE sbp_payments DROP COLUMN sent_confirmed;
ALTER TABLE sbp_payments RENAME COLUMN authorized TO confirmed;
