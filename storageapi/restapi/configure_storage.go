// This file is safe to edit. Once it exists it will not be overwritten

package restapi

import (
	"crypto/tls"
	"net/http"

	errors "github.com/go-openapi/errors"
	runtime "github.com/go-openapi/runtime"

	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
)

//go:generate swagger generate server --target ../../storageapi --name Storage --spec ../swagger.yml --api-package op --model-package model --exclude-main --strict

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

	api.AddServiceAmountHandler = op.AddServiceAmountHandlerFunc(func(params op.AddServiceAmountParams) op.AddServiceAmountResponder {
		return op.AddServiceAmountNotImplemented()
	})
	api.DelStationHandler = op.DelStationHandlerFunc(func(params op.DelStationParams) op.DelStationResponder {
		return op.DelStationNotImplemented()
	})
	api.GetPingHandler = op.GetPingHandlerFunc(func(params op.GetPingParams) op.GetPingResponder {
		return op.GetPingNotImplemented()
	})
	api.InfoHandler = op.InfoHandlerFunc(func(params op.InfoParams) op.InfoResponder {
		return op.InfoNotImplemented()
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
	api.PingHandler = op.PingHandlerFunc(func(params op.PingParams) op.PingResponder {
		return op.PingNotImplemented()
	})
	api.SaveHandler = op.SaveHandlerFunc(func(params op.SaveParams) op.SaveResponder {
		return op.SaveNotImplemented()
	})
	api.SaveCollectionHandler = op.SaveCollectionHandlerFunc(func(params op.SaveCollectionParams) op.SaveCollectionResponder {
		return op.SaveCollectionNotImplemented()
	})
	api.SaveMoneyHandler = op.SaveMoneyHandlerFunc(func(params op.SaveMoneyParams) op.SaveMoneyResponder {
		return op.SaveMoneyNotImplemented()
	})
	api.SaveRelayHandler = op.SaveRelayHandlerFunc(func(params op.SaveRelayParams) op.SaveRelayResponder {
		return op.SaveRelayNotImplemented()
	})
	api.SetStationHandler = op.SetStationHandlerFunc(func(params op.SetStationParams) op.SetStationResponder {
		return op.SetStationNotImplemented()
	})
	api.StationReportHandler = op.StationReportHandlerFunc(func(params op.StationReportParams) op.StationReportResponder {
		return op.StationReportNotImplemented()
	})
	api.StatusHandler = op.StatusHandlerFunc(func(params op.StatusParams) op.StatusResponder {
		return op.StatusNotImplemented()
	})
	api.StatusCollectionHandler = op.StatusCollectionHandlerFunc(func(params op.StatusCollectionParams) op.StatusCollectionResponder {
		return op.StatusCollectionNotImplemented()
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
