// This file is safe to edit. Once it exists it will not be overwritten

package restapi

import (
	"crypto/tls"
	"net/http"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"

	"github.com/DiaElectronics/lea-central-wash/storageapi"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
)

//go:generate swagger generate server --target ../../storageapi --name Storage --spec ../swagger.yml --api-package op --model-package model --principal github.com/DiaElectronics/lea-central-wash/storageapi.Profile --exclude-main --strict-responders

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

	api.UseSwaggerUI()
	// To continue using redoc as your UI, uncomment the following line
	// api.UseRedoc()

	api.JSONConsumer = runtime.JSONConsumer()

	api.JSONProducer = runtime.JSONProducer()

	// Applies when the "Pin" header is set
	if api.PinCodeAuth == nil {
		api.PinCodeAuth = func(token string) (*storageapi.Profile, error) {
			return nil, errors.NotImplemented("api key auth (pinCode) Pin from header param [Pin] has not yet been implemented")
		}
	}

	// Set your custom authorizer if needed. Default one is security.Authorized()
	// Expected interface runtime.Authorizer
	//
	// Example:
	// api.APIAuthorizer = security.Authorized()

	if api.AddServiceAmountHandler == nil {
		api.AddServiceAmountHandler = op.AddServiceAmountHandlerFunc(func(params op.AddServiceAmountParams) op.AddServiceAmountResponder {
			return op.AddServiceAmountNotImplemented()
		})
	}
	if api.CardReaderConfigHandler == nil {
		api.CardReaderConfigHandler = op.CardReaderConfigHandlerFunc(func(params op.CardReaderConfigParams) op.CardReaderConfigResponder {
			return op.CardReaderConfigNotImplemented()
		})
	}
	if api.CardReaderConfigByHashHandler == nil {
		api.CardReaderConfigByHashHandler = op.CardReaderConfigByHashHandlerFunc(func(params op.CardReaderConfigByHashParams) op.CardReaderConfigByHashResponder {
			return op.CardReaderConfigByHashNotImplemented()
		})
	}
	if api.CreateUserHandler == nil {
		api.CreateUserHandler = op.CreateUserHandlerFunc(func(params op.CreateUserParams, principal *storageapi.Profile) op.CreateUserResponder {
			return op.CreateUserNotImplemented()
		})
	}
	if api.DelStationHandler == nil {
		api.DelStationHandler = op.DelStationHandlerFunc(func(params op.DelStationParams) op.DelStationResponder {
			return op.DelStationNotImplemented()
		})
	}
	if api.DeleteUserHandler == nil {
		api.DeleteUserHandler = op.DeleteUserHandlerFunc(func(params op.DeleteUserParams, principal *storageapi.Profile) op.DeleteUserResponder {
			return op.DeleteUserNotImplemented()
		})
	}
	if api.GetPingHandler == nil {
		api.GetPingHandler = op.GetPingHandlerFunc(func(params op.GetPingParams) op.GetPingResponder {
			return op.GetPingNotImplemented()
		})
	}
	if api.GetUserHandler == nil {
		api.GetUserHandler = op.GetUserHandlerFunc(func(params op.GetUserParams, principal *storageapi.Profile) op.GetUserResponder {
			return op.GetUserNotImplemented()
		})
	}
	if api.GetUsersHandler == nil {
		api.GetUsersHandler = op.GetUsersHandlerFunc(func(params op.GetUsersParams, principal *storageapi.Profile) op.GetUsersResponder {
			return op.GetUsersNotImplemented()
		})
	}
	if api.InfoHandler == nil {
		api.InfoHandler = op.InfoHandlerFunc(func(params op.InfoParams) op.InfoResponder {
			return op.InfoNotImplemented()
		})
	}
	if api.KasseHandler == nil {
		api.KasseHandler = op.KasseHandlerFunc(func(params op.KasseParams) op.KasseResponder {
			return op.KasseNotImplemented()
		})
	}
	if api.LoadHandler == nil {
		api.LoadHandler = op.LoadHandlerFunc(func(params op.LoadParams) op.LoadResponder {
			return op.LoadNotImplemented()
		})
	}
	if api.LoadFromStationHandler == nil {
		api.LoadFromStationHandler = op.LoadFromStationHandlerFunc(func(params op.LoadFromStationParams) op.LoadFromStationResponder {
			return op.LoadFromStationNotImplemented()
		})
	}
	if api.LoadMoneyHandler == nil {
		api.LoadMoneyHandler = op.LoadMoneyHandlerFunc(func(params op.LoadMoneyParams) op.LoadMoneyResponder {
			return op.LoadMoneyNotImplemented()
		})
	}
	if api.LoadRelayHandler == nil {
		api.LoadRelayHandler = op.LoadRelayHandlerFunc(func(params op.LoadRelayParams) op.LoadRelayResponder {
			return op.LoadRelayNotImplemented()
		})
	}
	if api.OpenStationHandler == nil {
		api.OpenStationHandler = op.OpenStationHandlerFunc(func(params op.OpenStationParams) op.OpenStationResponder {
			return op.OpenStationNotImplemented()
		})
	}
	if api.PingHandler == nil {
		api.PingHandler = op.PingHandlerFunc(func(params op.PingParams) op.PingResponder {
			return op.PingNotImplemented()
		})
	}
	if api.PressButtonHandler == nil {
		api.PressButtonHandler = op.PressButtonHandlerFunc(func(params op.PressButtonParams) op.PressButtonResponder {
			return op.PressButtonNotImplemented()
		})
	}
	if api.ProgramsHandler == nil {
		api.ProgramsHandler = op.ProgramsHandlerFunc(func(params op.ProgramsParams) op.ProgramsResponder {
			return op.ProgramsNotImplemented()
		})
	}
	if api.RunProgramHandler == nil {
		api.RunProgramHandler = op.RunProgramHandlerFunc(func(params op.RunProgramParams) op.RunProgramResponder {
			return op.RunProgramNotImplemented()
		})
	}
	if api.SaveHandler == nil {
		api.SaveHandler = op.SaveHandlerFunc(func(params op.SaveParams) op.SaveResponder {
			return op.SaveNotImplemented()
		})
	}
	if api.SaveCollectionHandler == nil {
		api.SaveCollectionHandler = op.SaveCollectionHandlerFunc(func(params op.SaveCollectionParams, principal *storageapi.Profile) op.SaveCollectionResponder {
			return op.SaveCollectionNotImplemented()
		})
	}
	if api.SaveIfNotExistsHandler == nil {
		api.SaveIfNotExistsHandler = op.SaveIfNotExistsHandlerFunc(func(params op.SaveIfNotExistsParams) op.SaveIfNotExistsResponder {
			return op.SaveIfNotExistsNotImplemented()
		})
	}
	if api.SaveMoneyHandler == nil {
		api.SaveMoneyHandler = op.SaveMoneyHandlerFunc(func(params op.SaveMoneyParams) op.SaveMoneyResponder {
			return op.SaveMoneyNotImplemented()
		})
	}
	if api.SaveRelayHandler == nil {
		api.SaveRelayHandler = op.SaveRelayHandlerFunc(func(params op.SaveRelayParams) op.SaveRelayResponder {
			return op.SaveRelayNotImplemented()
		})
	}
	if api.SaveStationEventHandler == nil {
		api.SaveStationEventHandler = op.SaveStationEventHandlerFunc(func(params op.SaveStationEventParams) op.SaveStationEventResponder {
			return op.SaveStationEventNotImplemented()
		})
	}
	if api.SetCardReaderConfigHandler == nil {
		api.SetCardReaderConfigHandler = op.SetCardReaderConfigHandlerFunc(func(params op.SetCardReaderConfigParams) op.SetCardReaderConfigResponder {
			return op.SetCardReaderConfigNotImplemented()
		})
	}
	if api.SetKasseHandler == nil {
		api.SetKasseHandler = op.SetKasseHandlerFunc(func(params op.SetKasseParams) op.SetKasseResponder {
			return op.SetKasseNotImplemented()
		})
	}
	if api.SetProgramHandler == nil {
		api.SetProgramHandler = op.SetProgramHandlerFunc(func(params op.SetProgramParams) op.SetProgramResponder {
			return op.SetProgramNotImplemented()
		})
	}
	if api.SetStationHandler == nil {
		api.SetStationHandler = op.SetStationHandlerFunc(func(params op.SetStationParams) op.SetStationResponder {
			return op.SetStationNotImplemented()
		})
	}
	if api.SetStationButtonHandler == nil {
		api.SetStationButtonHandler = op.SetStationButtonHandlerFunc(func(params op.SetStationButtonParams) op.SetStationButtonResponder {
			return op.SetStationButtonNotImplemented()
		})
	}
	if api.StationHandler == nil {
		api.StationHandler = op.StationHandlerFunc(func(params op.StationParams) op.StationResponder {
			return op.StationNotImplemented()
		})
	}
	if api.StationButtonHandler == nil {
		api.StationButtonHandler = op.StationButtonHandlerFunc(func(params op.StationButtonParams) op.StationButtonResponder {
			return op.StationButtonNotImplemented()
		})
	}
	if api.StationByHashHandler == nil {
		api.StationByHashHandler = op.StationByHashHandlerFunc(func(params op.StationByHashParams) op.StationByHashResponder {
			return op.StationByHashNotImplemented()
		})
	}
	if api.StationCollectionReportDatesHandler == nil {
		api.StationCollectionReportDatesHandler = op.StationCollectionReportDatesHandlerFunc(func(params op.StationCollectionReportDatesParams, principal *storageapi.Profile) op.StationCollectionReportDatesResponder {
			return op.StationCollectionReportDatesNotImplemented()
		})
	}
	if api.StationEventsReportDatesHandler == nil {
		api.StationEventsReportDatesHandler = op.StationEventsReportDatesHandlerFunc(func(params op.StationEventsReportDatesParams) op.StationEventsReportDatesResponder {
			return op.StationEventsReportDatesNotImplemented()
		})
	}
	if api.StationProgramByHashHandler == nil {
		api.StationProgramByHashHandler = op.StationProgramByHashHandlerFunc(func(params op.StationProgramByHashParams) op.StationProgramByHashResponder {
			return op.StationProgramByHashNotImplemented()
		})
	}
	if api.StationReportCurrentMoneyHandler == nil {
		api.StationReportCurrentMoneyHandler = op.StationReportCurrentMoneyHandlerFunc(func(params op.StationReportCurrentMoneyParams) op.StationReportCurrentMoneyResponder {
			return op.StationReportCurrentMoneyNotImplemented()
		})
	}
	if api.StationReportDatesHandler == nil {
		api.StationReportDatesHandler = op.StationReportDatesHandlerFunc(func(params op.StationReportDatesParams) op.StationReportDatesResponder {
			return op.StationReportDatesNotImplemented()
		})
	}
	if api.StationsVariablesHandler == nil {
		api.StationsVariablesHandler = op.StationsVariablesHandlerFunc(func(params op.StationsVariablesParams) op.StationsVariablesResponder {
			return op.StationsVariablesNotImplemented()
		})
	}
	if api.StatusHandler == nil {
		api.StatusHandler = op.StatusHandlerFunc(func(params op.StatusParams) op.StatusResponder {
			return op.StatusNotImplemented()
		})
	}
	if api.StatusCollectionHandler == nil {
		api.StatusCollectionHandler = op.StatusCollectionHandlerFunc(func(params op.StatusCollectionParams, principal *storageapi.Profile) op.StatusCollectionResponder {
			return op.StatusCollectionNotImplemented()
		})
	}
	if api.UpdateUserHandler == nil {
		api.UpdateUserHandler = op.UpdateUserHandlerFunc(func(params op.UpdateUserParams, principal *storageapi.Profile) op.UpdateUserResponder {
			return op.UpdateUserNotImplemented()
		})
	}
	if api.UpdateUserPasswordHandler == nil {
		api.UpdateUserPasswordHandler = op.UpdateUserPasswordHandlerFunc(func(params op.UpdateUserPasswordParams, principal *storageapi.Profile) op.UpdateUserPasswordResponder {
			return op.UpdateUserPasswordNotImplemented()
		})
	}

	api.PreServerShutdown = func() {}

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
// scheme value will be set accordingly: "http", "https" or "unix".
func configureServer(s *http.Server, scheme, addr string) {
}

// The middleware configuration is for the handler executors. These do not apply to the swagger.json document.
// The middleware executes after routing but before authentication, binding and validation.
func setupMiddlewares(handler http.Handler) http.Handler {
	return handler
}

// The middleware configuration happens before anything, this middleware also applies to serving the swagger.json document.
// So this is a good place to plug in a panic handling middleware, logging and metrics.
func setupGlobalMiddleware(handler http.Handler) http.Handler {
	return handler
}
