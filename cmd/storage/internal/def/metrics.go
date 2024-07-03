package def

import (
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
)

// Metric provides access to global metrics used by all packages.
var Metric struct {
	PanicsTotal prometheus.Counter
}

// InitMetrics must be called once before using this package.
// It registers and initializes metrics used by this package.
func InitMetrics(namespace string) {
	Metric.PanicsTotal = promauto.NewCounter(
		prometheus.CounterOpts{
			Namespace: namespace,
			Name:      "panics_total",
			Help:      "Amount of recovered panics.",
		},
	)
}
