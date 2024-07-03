package sbpclient

import (
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
)

const (
	msgTypeLabel = "msg_type"
)

// Metric provides access to global metrics used by all packages.
var Metric struct {
	ErrsTotal           prometheus.Counter
	ReconnectTotal      prometheus.Counter
	PublicationsTotal   *prometheus.CounterVec
	DeliveriesTotal     *prometheus.CounterVec
	DeliveriesErrsTotal *prometheus.CounterVec
	DeliveriesDuration  *prometheus.HistogramVec
}

// InitMetrics must be called once before using this package.
// It registers and initializes metrics used by this package.
func InitMetrics(namespace string) {
	Metric.ErrsTotal = promauto.NewCounter(prometheus.CounterOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "errs_total",
		Help:      "Amount of errs.",
	})
	Metric.ReconnectTotal = promauto.NewCounter(prometheus.CounterOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "reconnect_total",
		Help:      "Amount of reconnects.",
	})
	Metric.PublicationsTotal = promauto.NewCounterVec(prometheus.CounterOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "publications_total",
		Help:      "Amount of publications.",
	}, []string{msgTypeLabel})
	Metric.DeliveriesTotal = promauto.NewCounterVec(prometheus.CounterOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "deliveries_total",
		Help:      "Amount of deliveries.",
	}, []string{msgTypeLabel})
	Metric.DeliveriesDuration = promauto.NewHistogramVec(prometheus.HistogramOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "deliveries_duration",
		Help:      "Duration of deliveries.",
	}, []string{msgTypeLabel})
	Metric.DeliveriesErrsTotal = promauto.NewCounterVec(prometheus.CounterOpts{
		Namespace: namespace,
		Subsystem: "rabbit_kaspi",
		Name:      "deliveries_errs_total",
		Help:      "Amount of deliveries errs.",
	}, []string{msgTypeLabel})
}
