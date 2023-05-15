package app

import "fmt"

func CheckAccess(auth *Auth, accessRules ...func(*Auth) bool) bool {
	var ok bool
	for _, rule := range accessRules {
		ok = rule(auth)
		if !ok {
			return false
		}
	}

	return true
}

func allRules(accessRules ...func(*Auth) bool) func(auth *Auth) bool {
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

func anyRule(accessRule ...func(*Auth) bool) func(auth *Auth) bool {
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
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Unexpected nil auth in checking roleAdmin, ", r)
		}
		ok = false
	}()

	ok = auth.IsAdmin

	return
}

func roleOperator(auth *Auth) (ok bool) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Unexpected nil auth in checking roleOperator, ", r)
		}
		ok = false
	}()

	ok = auth.IsOperator

	return
}

func roleEngineer(auth *Auth) (ok bool) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Unexpected nil auth in checking roleEngineer, ", r)
		}
		ok = false
	}()

	ok = auth.IsEngineer

	return
}
