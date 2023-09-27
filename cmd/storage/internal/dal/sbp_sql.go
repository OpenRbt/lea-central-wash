package dal

const (
	// save
	SavePayment = `
	INSERT INTO public.sbp_payments 
	(server_id, post_id, order_id, url_pay, amount, canceled, confirmed, openwash_received, created_at, updated_at)
	VALUES (:server_id, :post_id, :order_id, :url_pay, :amount, :canceled, :confirmed, :openwash_received, :created_at, :updated_at)
	RETURNING id;
	`
	// set
	SetPaymentURL = `
	UPDATE public.sbp_payments
	SET url_pay = :url_pay, updated_at = :updated_at
	WHERE order_id = :order_id;
	`
	SetPaymentConfirmed = `
	UPDATE public.sbp_payments
	SET confirmed = true, updated_at = :updated_at
	WHERE order_id = :order_id;
	`
	SetPaymentReceived = `
	UPDATE public.sbp_payments
	SET openwash_received = true, updated_at = :updated_at
	WHERE order_id = :order_id;
	`
	SetPaymentCanceled = `
	UPDATE public.sbp_payments
	SET canceled = true, updated_at = :updated_at
	WHERE order_id = :order_id;
	`
	// get
	GetLastPayment = `
	SELECT *
	FROM public.sbp_payments
	WHERE post_id = :post_id
	ORDER BY created_at DESC
	LIMIT 1;
	`
	GetPaymentByOrderID = `
	SELECT *
	FROM public.sbp_payments
	WHERE order_id = :order_id;
	`
	GetActualPayments = `
	SELECT *
	FROM public.sbp_payments
	WHERE canceled != true
    AND openwash_received != true
	ORDER BY post_id, created_at DESC
	`
)
