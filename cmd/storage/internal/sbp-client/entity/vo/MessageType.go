package vo

type MessageType string

const (
	MessageTypePaymentRequest  MessageType = "sbp_client_service/payment_request"
	MessageTypePaymentResponse MessageType = "sbp_client_service/payment_response"

	MessageTypePaymentNotification MessageType = "sbp_client_service/payment_notification"

	MessageTypePaymentCancellationRequest MessageType = "sbp_client_service/payment_cancellation_request"
	PaymentConfirmationRequestMessageType MessageType = "sbp_client_service/payment_confirmation_request"

	SBPPingMessageType MessageType = "sbp_client_service/ping"

	ServiceStatusMessageType        MessageType = "sbp_client_service/service_status"
	ServiceStatusRequestMessageType MessageType = "sbp_client_service/service_status_request"
)
