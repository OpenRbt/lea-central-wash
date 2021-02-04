package auth

import (
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	oapierrors "github.com/go-openapi/errors"
	"github.com/powerman/structlog"
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
	user, err := a.app.User(token)
	if err != nil {
		return nil, oapierrors.Unauthenticated("invalid credentials")
	}

	return &app.Auth{
		ID:         user.ID,
		Login:      user.Login,
		FirstName:  user.FirstName,
		MiddleName: user.MiddleName,
		LastName:   user.LastName,
		IsAdmin:    user.IsAdmin,
		IsOperator: user.IsOperator,
		IsEngineer: user.IsEngineer,
	}, nil
}
