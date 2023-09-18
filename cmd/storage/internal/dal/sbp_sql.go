package dal

const (
	// save
	SavePayment = `
	INSERT INTO public.sbp_payments 
	(server_id, post_id, order_id, url_pay, amount, canceled, confirmed, openwash_received, created_at, updated_at)
	VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
	RETURNING id;
	`
	// set
	SetPaymentURL = `
	UPDATE public.sbp_payments
	SET url_pay = $1, updated_at = NOW()
	WHERE order_id = $2;
	`
	SetPaymentConfirmed = `
	UPDATE public.sbp_payments
	SET confirmed = true, updated_at = NOW()
	WHERE order_id = $1;
	`
	SetPaymentReceived = `
	UPDATE public.sbp_payments
	SET openwash_received = true, updated_at = NOW()
	WHERE order_id = $1;
	`
	SetPaymentCanceled = `
	UPDATE public.sbp_payments
	SET canceled = true, updated_at = NOW()
	WHERE order_id = $1;
	`
	// get
	GetLastPayment = `
	SELECT *
	FROM public.sbp_payments
	WHERE post_id = $1
	ORDER BY created_at DESC
	LIMIT 1;
	`
	GetPaymentByOrderID = `
	SELECT *
	FROM public.sbp_payments
	WHERE order_id = $1;
	`
	GetActualPayments = `
	SELECT *
	FROM public.sbp_payments
	WHERE canceled != true
    AND openwash_received != true
	ORDER BY post_id, created_at DESC
	`
)
