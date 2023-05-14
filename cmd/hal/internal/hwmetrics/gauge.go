package hwmetrics

import "github.com/prometheus/client_golang/prometheus"

type GaugeMetric struct {
	object *prometheus.GaugeVec
}

func NewGaugeMetric(newPromGauge *prometheus.GaugeVec) GaugeMetric {
	return GaugeMetric{
		object: newPromGauge,
	}
}

func (g GaugeMetric) SetGaugeByID(id string, newValue float64) {
	g.object.WithLabelValues(id).Set(newValue)
}
