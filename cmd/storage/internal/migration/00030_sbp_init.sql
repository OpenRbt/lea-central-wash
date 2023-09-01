-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE public.sbp_payments (
	id bigserial NOT NULL,
	server_id uuid NOT NULL,
	post_id int4 NOT NULL,
	order_id uuid NULL,
	url_pay varchar NULL,
	amount int8 NULL,
	canceled bool NULL,
	confirmed bool NULL,
	openwash_received bool NULL,
	created_at timestamp NOT NULL,
	updated_at timestamp NULL,
	CONSTRAINT sbp_payments_pk PRIMARY KEY (id),
	CONSTRAINT sbp_payments_un UNIQUE (order_id)
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE public.sbp_payments;