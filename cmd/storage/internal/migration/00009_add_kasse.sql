-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE kasse (
    id SERIAL PRIMARY KEY,
    ctime TIMESTAMP DEFAULT NOW(),
    receipt_item TEXT NOT NULL,
    tax_type TEXT NOT NULL,
    cashier_full_name TEXT NOT NULL,
    cashier_inn TEXT NOT NULL
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
DROP TABLE kasse;
