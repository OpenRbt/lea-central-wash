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
	case BonusSessionUserAssign:
		return "bonus_svc/user_assign"
	case BonusSessionBonusCharge:
		return "bonus_svc/bonus_charge"
	case BonusSessionEventLog:
		return "bonus_svc/event_log"
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
	case "bonus_svc/session_start":
		return BonusSessionStart
	case "bonus_svc/session_finish":
		return BonusSessionFinish
	case "bonus_svc/user_assign":
		return BonusSessionUserAssign
	case "bonus_svc/bonus_charge":
		return BonusSessionBonusCharge
	case "bonus_svc/event_log":
		return BonusSessionEventLog
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
)
