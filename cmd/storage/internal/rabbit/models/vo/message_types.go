package vo

type MessageType int

func (m MessageType) String() string {
	switch m {
	case BonusSessionRequest:
		return "bonus_svc/session_request"
	case BonusSessionCreated:
		return "bonus_svc/session_created"
	case BonusSessionStart:
		return "bonus_svc/session_start"
	case BonusSessionFinish:
		return "bonus_svc/session_finish"
	case BonusSessionBonusCharge:
		return "bonus_svc/session_bonus_charge"
	case BonusSessionBonusConfirm:
		return "bonus_svc/session_bonus_confirm"
	case BonusSessionBonusDiscard:
		return "bonus_svc/session_bonus_discard"
	case BonusSessionStateChange:
		return "bonus_svc/session_state_change"
	case BonusSessionUserAssign:
		return "bonus_svc/user_assign"
	case BonusSessionEventLog:
		return "bonus_svc/event_log"
	case WashAdminServerRegistered:
		return "wash_admin/server_registered"
	case WashAdminServerUpdated:
		return "wash_admin/server_updated"
	default:
		return "unknown"
	}
}

func MessageTypeFromString(contentType string) MessageType {
	switch contentType {
	case "bonus_svc/session_request":
		return BonusSessionRequest
	case "bonus_svc/session_created":
		return BonusSessionCreated
	case "bonus_svc/session_bonus_charge":
		return BonusSessionBonusCharge
	case "bonus_svc/session_bonus_confirm":
		return BonusSessionBonusConfirm
	case "bonus_svc/session_bonus_discard":
		return BonusSessionBonusDiscard
	case "bonus_svc/session_state_change":
		return BonusSessionStateChange
	case "bonus_svc/session_start":
		return BonusSessionStart
	case "bonus_svc/session_finish":
		return BonusSessionFinish
	case "bonus_svc/user_assign":
		return BonusSessionUserAssign
	case "bonus_svc/event_log":
		return BonusSessionEventLog
	case "wash_admin/server_registered":
		return WashAdminServerRegistered
	case "wash_admin/server_updated":
		return WashAdminServerUpdated
	default:
		return MessageTypeUnknown
	}
}

const (
	MessageTypeUnknown MessageType = iota - 1
	BonusSessionRequest
	BonusSessionCreated
	BonusSessionStart
	BonusSessionUserAssign
	BonusSessionBonusCharge
	BonusSessionFinish
	BonusSessionEventLog

	BonusSessionStateChange

	BonusSessionBonusConfirm
	BonusSessionBonusDiscard

	WashAdminServerRegistered
	WashAdminServerUpdated
)
