package extapi

import (
	"encoding/json"
	"strconv"

	"github.com/go-openapi/loads"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
)

//nolint:gochecknoglobals
var metric struct {
	reqInFlight prometheus.Gauge
	reqTotal    *prometheus.CounterVec
	reqDuration *prometheus.HistogramVec
}

const (
	resourceLabel = "resource"
	methodLabel   = "method"
	codeLabel     = "code"
)

//nolint:gochecknoglobals
var (
	// Initialized with codes returned by swagger and middlewares
	// after metrics middleware (accessLog).
	codeLabels = []int{400, 401, 403, 404}
)

// InitMetrics must be called once before using this package.
// It registers and initializes metrics used by this package.
func InitMetrics(namespace string, flatSwaggerJSON json.RawMessage) {
	const subsystem = "extapi"

	metric.reqInFlight = promauto.NewGauge(
		prometheus.GaugeOpts{
			Namespace: namespace,
			Subsystem: subsystem,
			Name:      "http_requests_in_flight",
			Help:      "Amount of currently processing API requests.",
		},
	)
	metric.reqTotal = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Namespace: namespace,
			Subsystem: subsystem,
			Name:      "http_requests_total",
			Help:      "Amount of processed API requests.",
		},
		[]string{resourceLabel, methodLabel, codeLabel},
	)
	metric.reqDuration = promauto.NewHistogramVec(
		prometheus.HistogramOpts{
			Namespace: namespace,
			Subsystem: subsystem,
			Name:      "http_request_duration_seconds",
			Help:      "API request latency distributions.",
		},
		[]string{resourceLabel, methodLabel, codeLabel},
	)

	ss, err := loads.Analyzed(flatSwaggerJSON, "")
	if err != nil {
		panic(err)
	}
	for method, resources := range ss.Analyzer.Operations() {
		for resource, op := range resources {
			codes := append([]int{}, codeLabels...)
			for code := range op.Responses.StatusCodeResponses {
				codes = append(codes, code)
			}
			for _, code := range codes {
				l := prometheus.Labels{
					resourceLabel: resource,
					methodLabel:   method,
					codeLabel:     strconv.Itoa(code),
				}
				metric.reqTotal.With(l)
				metric.reqDuration.With(l)
			}
		}
	}
}
