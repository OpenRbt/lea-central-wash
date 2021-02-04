package storageapi

// Profile describes user profile.
type Profile struct {
	ID         int
	Login      string
	FirstName  string
	MiddleName string
	LastName   string
	IsAdmin    bool
	IsOperator bool
	IsEngineer bool
}
