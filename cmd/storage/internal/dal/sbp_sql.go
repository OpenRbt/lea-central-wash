package dal

const (
	savePayment = `
	INSERT INTO public.sbp_payments 
	(server_id, post_id, order_id, url_pay, amount, canceled, authorized, openwash_received, created_at, updated_at, confirmed, last_confirmed_at, sent_confirmed)
	VALUES (:server_id, :post_id, :order_id, :url_pay, :amount, :canceled, :authorized, :openwash_received, :created_at, :updated_at, :confirmed, :last_confirmed_at, :sent_confirmed)
	RETURNING id;
	`
	updatePayment = `
	UPDATE public.sbp_payments
	SET
		url_pay = COALESCE(:url_pay, url_pay),
		canceled = COALESCE(:canceled, canceled),
		authorized = COALESCE(:authorized, authorized),
		openwash_received = COALESCE(:openwash_received, openwash_received),
		confirmed = COALESCE(:confirmed, confirmed),
		sent_confirmed = COALESCE(:sent_confirmed, sent_confirmed),
		last_confirmed_at = COALESCE(:last_confirmed_at, last_confirmed_at),
		updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	setPaymentURL = `
	UPDATE public.sbp_payments
	SET url_pay = :url_pay, updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	setPaymentAuthorized = `
	UPDATE public.sbp_payments
	SET authorized = true, updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	setPaymentReceived = `
	UPDATE public.sbp_payments
	SET openwash_received = true, updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	setPaymentCanceled = `
	UPDATE public.sbp_payments
	SET canceled = true, updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	getLastPayment = `
	SELECT *
	FROM public.sbp_payments
	WHERE post_id = :post_id
	ORDER BY created_at DESC
	LIMIT 1;
	`
	getPaymentByOrderID = `
	SELECT *
	FROM public.sbp_payments
	WHERE order_id = :order_id;
	`
	getActualPayments = `
	SELECT *
	FROM public.sbp_payments
	WHERE canceled != true
    AND openwash_received != true
	ORDER BY post_id, created_at DESC
	`
	getPaymentsForConfirm = `
	SELECT *
	FROM public.sbp_payments
	WHERE NOT canceled
    AND openwash_received
	AND NOT confirmed
	ORDER BY post_id, created_at DESC
	`
	getPaymentsForConfirmAgain = `
	SELECT *
	FROM public.sbp_payments
	WHERE NOT canceled
    AND openwash_received
	AND NOT confirmed
	AND sent_confirmed
	ORDER BY post_id, created_at DESC
	`
)
