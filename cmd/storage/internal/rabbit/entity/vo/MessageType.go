package vo

type MessageType string

const (
	SessionRequestMessageType      MessageType = "bonus_service/session/request"
	SessionCreatedMessageType      MessageType = "bonus_service/session/created"
	SessionStartMessageType        MessageType = "bonus_service/session/start"
	SessionFinishMessageType       MessageType = "bonus_service/session/finish"
	SessionStateMessageType        MessageType = "bonus_service/session/state"
	SessionUserMessageType         MessageType = "bonus_service/session/user"
	SessionEventMessageType        MessageType = "bonus_service/session/event"
	SessionBonusChargeMessageType  MessageType = "bonus_service/session/bonus/charge"
	SessionBonusConfirmMessageType MessageType = "bonus_service/session/bonus/confirm"
	SessionBonusDiscardMessageType MessageType = "bonus_service/session/bonus/discard"
	SessionBonusRewardMessageType  MessageType = "bonus_service/session/bonus/reward"
	SessionMoneyReportMessageType  MessageType = "bonus_service/session/money-report"

	AdminServerRegisteredMessageType MessageType = "admin_service/server/registered"
	AdminServerUpdatedMessageType    MessageType = "admin_service/server/updated"
)
