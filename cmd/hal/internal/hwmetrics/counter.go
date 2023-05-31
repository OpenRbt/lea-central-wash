package hwmetrics

import "github.com/prometheus/client_golang/prometheus"

type CounterMetric struct {
	object *prometheus.CounterVec
}

func NewCounterMetric(newPromVec *prometheus.CounterVec) CounterMetric {
	return CounterMetric{
		object: newPromVec,
	}
}

func (c CounterMetric) Inc(id string) {
	c.object.WithLabelValues(id).Inc()
}
