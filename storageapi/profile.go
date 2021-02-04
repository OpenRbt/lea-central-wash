package storageapi

// Profile describes user profile.
type Profile struct {
	ID         int
	FirstName  string
	MiddleName string
	LastName   string
	UserRoles  []string
}
