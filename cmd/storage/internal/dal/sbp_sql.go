package dal

const (
	// save
	savePayment = `
	INSERT INTO public.sbp_payments 
	(server_id, post_id, order_id, url_pay, amount, canceled, confirmed, openwash_received, created_at, updated_at)
	VALUES (:server_id, :post_id, :order_id, :url_pay, :amount, :canceled, :confirmed, :openwash_received, :created_at, :updated_at)
	RETURNING id;
	`
	// set
	setPaymentURL = `
	UPDATE public.sbp_payments
	SET url_pay = :url_pay, updated_at = :updated_at
	WHERE order_id = :order_id
	RETURNING post_id;
	`
	setPaymentConfirmed = `
	UPDATE public.sbp_payments
	SET confirmed = true, updated_at = :updated_at
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
	// get
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
)
