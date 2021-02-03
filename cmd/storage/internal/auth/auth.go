package auth

import (
	"fmt"
	"time"

	oapierrors "github.com/go-openapi/errors"
	"github.com/powerman/structlog"
	"golang.org/x/crypto/bcrypt"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
)

var log = structlog.New() //nolint:gochecknoglobals

const (
	cookieExpire = 24 * 7 * time.Hour
)

// Check access to manipulation user token
type Check interface {
	CheckAuth(token string) (*app.Auth, error)
}

type check struct {
	log *structlog.Logger
	app app.App
}

// NewAuthCheck - create new authCheck.
func NewAuthCheck(log *structlog.Logger, appInstance app.App) Check {
	return &check{
		log: log,
		app: appInstance,
	}
}

// CheckAuth function for check token
func (a *check) CheckAuth(token string) (*app.Auth, error) { //nolint:gocyclo
	/*
		user, err := a.app.GetUserByToken(context.Background(), a.log, token)
		if err != nil {
			return nil, err
		}
	*/
	users, err := a.app.Users()
	if err != nil {
		return nil, err
	}

	for u := range users {
		user := users[u]
		if errPassword := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(token)); user.Enabled == true && errPassword == nil {
			roles, errRoles := a.app.UserRoles(user)
			if errRoles != nil {
				return nil, errRoles
			}
			log.Info(fmt.Sprintf("Authenticated as: %s %s %s %v", user.FirstName, user.MiddleName, user.LastName, roles))
			return &app.Auth{
				FirstName:  user.FirstName,
				MiddleName: user.MiddleName,
				LastName:   user.LastName,
				UserRoles:  roles,
			}, nil
		}
	}

	return nil, oapierrors.Unauthenticated("invalid credentials")
}
