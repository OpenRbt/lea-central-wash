package app

type AccessRule func(*Auth) bool

func CheckAccess(auth *Auth, rule AccessRule) bool {
	if rule == nil {
		return auth != nil
	}

	return rule(auth)
}

func allRules(accessRules ...func(*Auth) bool) AccessRule {
	return func(auth *Auth) bool {
		var ok bool

		for _, rule := range accessRules {
			ok = rule(auth)
			if !ok {
				return false
			}
		}

		return true
	}
}

func anyRule(accessRule ...func(*Auth) bool) AccessRule {
	return func(auth *Auth) bool {
		var ok bool

		for _, rule := range accessRule {
			ok = rule(auth)
			if ok {
				return ok
			}
		}

		return false
	}
}

func allowUnauthorized(auth *Auth) bool {
	return true
}

func roleAdmin(auth *Auth) (ok bool) {
	if auth == nil {
		return false
	}

	ok = auth.IsAdmin

	return
}

func roleOperator(auth *Auth) (ok bool) {
	if auth == nil {
		return false
	}

	ok = auth.IsOperator

	return
}

func roleEngineer(auth *Auth) (ok bool) {
	if auth == nil {
		return false
	}

	ok = auth.IsEngineer

	return
}
