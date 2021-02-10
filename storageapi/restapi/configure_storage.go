// This file is safe to edit. Once it exists it will not be overwritten

package restapi

import (
	"crypto/tls"
	"net/http"

	errors "github.com/go-openapi/errors"
	runtime "github.com/go-openapi/runtime"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
)

//go:generate swagger generate server --target ../../storageapi --name Storage --spec ../swagger.yml --api-package op --model-package model --principal storageapi.Profile --exclude-main --strict

func configureFlags(api *op.StorageAPI) {
	// api.CommandLineOptionsGroups = []swag.CommandLineOptionsGroup{ ... }
}

func configureAPI(api *op.StorageAPI) http.Handler {
	// configure the api here
	api.ServeError = errors.ServeError

	// Set your custom logger if needed. Default one is log.Printf
	// Expected interface func(string, ...interface{})
	//
	// Example:
	// api.Logger = log.Printf

	api.JSONConsumer = runtime.JSONConsumer()

	api.JSONProducer = runtime.JSONProducer()

	// Applies when the "Pin" header is set
	api.PinCodeAuth = func(token string) (*storageapi.Profile, error) {
		return nil, errors.NotImplemented("api key auth (pinCode) Pin from header param [Pin] has not yet been implemented")
	}

	// Set your custom authorizer if needed. Default one is security.Authorized()
	// Expected interface runtime.Authorizer
	//
	// Example:
	// api.APIAuthorizer = security.Authorized()
	api.AddServiceAmountHandler = op.AddServiceAmountHandlerFunc(func(params op.AddServiceAmountParams) op.AddServiceAmountResponder {
		return op.AddServiceAmountNotImplemented()
	})
	api.CardReaderConfigHandler = op.CardReaderConfigHandlerFunc(func(params op.CardReaderConfigParams) op.CardReaderConfigResponder {
		return op.CardReaderConfigNotImplemented()
	})
	api.CardReaderConfigByHashHandler = op.CardReaderConfigByHashHandlerFunc(func(params op.CardReaderConfigByHashParams) op.CardReaderConfigByHashResponder {
		return op.CardReaderConfigByHashNotImplemented()
	})
	api.CreateUserHandler = op.CreateUserHandlerFunc(func(params op.CreateUserParams, principal *storageapi.Profile) op.CreateUserResponder {
		return op.CreateUserNotImplemented()
	})
	api.DelStationHandler = op.DelStationHandlerFunc(func(params op.DelStationParams) op.DelStationResponder {
		return op.DelStationNotImplemented()
	})
	api.DeleteUserHandler = op.DeleteUserHandlerFunc(func(params op.DeleteUserParams, principal *storageapi.Profile) op.DeleteUserResponder {
		return op.DeleteUserNotImplemented()
	})
	api.GetPingHandler = op.GetPingHandlerFunc(func(params op.GetPingParams) op.GetPingResponder {
		return op.GetPingNotImplemented()
	})
	api.GetUserHandler = op.GetUserHandlerFunc(func(params op.GetUserParams, principal *storageapi.Profile) op.GetUserResponder {
		return op.GetUserNotImplemented()
	})
	api.GetUsersHandler = op.GetUsersHandlerFunc(func(params op.GetUsersParams, principal *storageapi.Profile) op.GetUsersResponder {
		return op.GetUsersNotImplemented()
	})
	api.InfoHandler = op.InfoHandlerFunc(func(params op.InfoParams) op.InfoResponder {
		return op.InfoNotImplemented()
	})
	api.KasseHandler = op.KasseHandlerFunc(func(params op.KasseParams) op.KasseResponder {
		return op.KasseNotImplemented()
	})
	api.LoadHandler = op.LoadHandlerFunc(func(params op.LoadParams) op.LoadResponder {
		return op.LoadNotImplemented()
	})
	api.LoadMoneyHandler = op.LoadMoneyHandlerFunc(func(params op.LoadMoneyParams) op.LoadMoneyResponder {
		return op.LoadMoneyNotImplemented()
	})
	api.LoadRelayHandler = op.LoadRelayHandlerFunc(func(params op.LoadRelayParams) op.LoadRelayResponder {
		return op.LoadRelayNotImplemented()
	})
	api.OpenStationHandler = op.OpenStationHandlerFunc(func(params op.OpenStationParams) op.OpenStationResponder {
		return op.OpenStationNotImplemented()
	})
	api.PingHandler = op.PingHandlerFunc(func(params op.PingParams) op.PingResponder {
		return op.PingNotImplemented()
	})
	api.ProgramRelaysHandler = op.ProgramRelaysHandlerFunc(func(params op.ProgramRelaysParams) op.ProgramRelaysResponder {
		return op.ProgramRelaysNotImplemented()
	})
	api.ProgramsHandler = op.ProgramsHandlerFunc(func(params op.ProgramsParams) op.ProgramsResponder {
		return op.ProgramsNotImplemented()
	})
	api.SaveHandler = op.SaveHandlerFunc(func(params op.SaveParams) op.SaveResponder {
		return op.SaveNotImplemented()
	})
	api.SaveCollectionHandler = op.SaveCollectionHandlerFunc(func(params op.SaveCollectionParams, principal *storageapi.Profile) op.SaveCollectionResponder {
		return op.SaveCollectionNotImplemented()
	})
	api.SaveIfNotExistsHandler = op.SaveIfNotExistsHandlerFunc(func(params op.SaveIfNotExistsParams) op.SaveIfNotExistsResponder {
		return op.SaveIfNotExistsNotImplemented()
	})
	api.SaveMoneyHandler = op.SaveMoneyHandlerFunc(func(params op.SaveMoneyParams) op.SaveMoneyResponder {
		return op.SaveMoneyNotImplemented()
	})
	api.SaveRelayHandler = op.SaveRelayHandlerFunc(func(params op.SaveRelayParams) op.SaveRelayResponder {
		return op.SaveRelayNotImplemented()
	})
	api.SetCardReaderConfigHandler = op.SetCardReaderConfigHandlerFunc(func(params op.SetCardReaderConfigParams) op.SetCardReaderConfigResponder {
		return op.SetCardReaderConfigNotImplemented()
	})
	api.SetKasseHandler = op.SetKasseHandlerFunc(func(params op.SetKasseParams) op.SetKasseResponder {
		return op.SetKasseNotImplemented()
	})
	api.SetProgramNameHandler = op.SetProgramNameHandlerFunc(func(params op.SetProgramNameParams) op.SetProgramNameResponder {
		return op.SetProgramNameNotImplemented()
	})
	api.SetProgramRelaysHandler = op.SetProgramRelaysHandlerFunc(func(params op.SetProgramRelaysParams) op.SetProgramRelaysResponder {
		return op.SetProgramRelaysNotImplemented()
	})
	api.SetStationHandler = op.SetStationHandlerFunc(func(params op.SetStationParams) op.SetStationResponder {
		return op.SetStationNotImplemented()
	})
	api.StationByHashHandler = op.StationByHashHandlerFunc(func(params op.StationByHashParams) op.StationByHashResponder {
		return op.StationByHashNotImplemented()
	})
	api.StationReportCurrentMoneyHandler = op.StationReportCurrentMoneyHandlerFunc(func(params op.StationReportCurrentMoneyParams) op.StationReportCurrentMoneyResponder {
		return op.StationReportCurrentMoneyNotImplemented()
	})
	api.StationReportDatesHandler = op.StationReportDatesHandlerFunc(func(params op.StationReportDatesParams) op.StationReportDatesResponder {
		return op.StationReportDatesNotImplemented()
	})
	api.StationsVariablesHandler = op.StationsVariablesHandlerFunc(func(params op.StationsVariablesParams) op.StationsVariablesResponder {
		return op.StationsVariablesNotImplemented()
	})
	api.StatusHandler = op.StatusHandlerFunc(func(params op.StatusParams) op.StatusResponder {
		return op.StatusNotImplemented()
	})
	api.StatusCollectionHandler = op.StatusCollectionHandlerFunc(func(params op.StatusCollectionParams, principal *storageapi.Profile) op.StatusCollectionResponder {
		return op.StatusCollectionNotImplemented()
	})
	api.UpdateUserHandler = op.UpdateUserHandlerFunc(func(params op.UpdateUserParams, principal *storageapi.Profile) op.UpdateUserResponder {
		return op.UpdateUserNotImplemented()
	})
	api.UpdateUserPasswordHandler = op.UpdateUserPasswordHandlerFunc(func(params op.UpdateUserPasswordParams, principal *storageapi.Profile) op.UpdateUserPasswordResponder {
		return op.UpdateUserPasswordNotImplemented()
	})

	api.ServerShutdown = func() {}

	return setupGlobalMiddleware(api.Serve(setupMiddlewares))
}

// The TLS configuration before HTTPS server starts.
func configureTLS(tlsConfig *tls.Config) {
	// Make all necessary changes to the TLS configuration here.
}

// As soon as server is initialized but not run yet, this function will be called.
// If you need to modify a config, store server instance to stop it individually later, this is the place.
// This function can be called multiple times, depending on the number of serving schemes.
// scheme value will be set accordingly: "http", "https" or "unix"
func configureServer(s *http.Server, scheme, addr string) {
}

// The middleware configuration is for the handler executors. These do not apply to the swagger.json document.
// The middleware executes after routing but before authentication, binding and validation
func setupMiddlewares(handler http.Handler) http.Handler {
	return handler
}

// The middleware configuration happens before anything, this middleware also applies to serving the swagger.json document.
// So this is a good place to plug in a panic handling middleware, logging and metrics
func setupGlobalMiddleware(handler http.Handler) http.Handler {
	return handler
}
