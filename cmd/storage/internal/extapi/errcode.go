package extapi

type errCode struct {
	status int
	extra  int32
}

func newErrCode(statusCode int, extraCode int32) errCode {
	// codeLabels = append(codeLabels, statusCode)
	if extraCode == 0 {
		extraCode = int32(statusCode)
	}
	return errCode{status: statusCode, extra: extraCode}
}

//nolint:gochecknoglobals
var (
	codeInternal              = newErrCode(500, 0)
	codeValidate              = newErrCode(422, 0)
	codeBadGateway            = newErrCode(502, 0)
	codeNotFound              = newErrCode(404, 0)
	codeTooManyRequests       = newErrCode(429, 0)
	codeManyRequest           = newErrCode(429, 0)
	codeForbidden             = newErrCode(403, 0)
	codeEmailUsed             = newErrCode(409, 1600)
	codeAccountNotAvailable   = newErrCode(409, 1601)
	codePassTooWeak           = newErrCode(422, 1602)
	codeWrongCaptcha          = newErrCode(422, 1603)
	codeCodeNotAvailable      = newErrCode(409, 1604)
	codeNoSuchEmailOrUsername = newErrCode(404, 1605)
	codeInvalidPass           = newErrCode(403, 1606)
	codeUserBlocked           = newErrCode(403, 1607)
	codeTooManyPassReset      = newErrCode(409, 1608)
	codeUniqueRoleName        = newErrCode(422, 1609)
	codeUniqueRoleCode        = newErrCode(422, 1610)
	codeRoleUsed              = newErrCode(422, 1611)
	codeInvalidPassResetToken = newErrCode(403, 1612)
)
