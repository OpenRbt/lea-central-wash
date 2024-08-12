package extapi

import (
	"net"
	"net/http"
	"strconv"
	"strings"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"
	"github.com/felixge/httpsnoop"
	"github.com/prometheus/client_golang/prometheus"

	"github.com/powerman/structlog"
)

type middlewareFunc func(http.Handler) http.Handler

func recovery(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			const code = http.StatusInternalServerError
			switch err := recover(); err := err.(type) {
			default:
				def.Metric.PanicsTotal.Inc()
				log := structlog.FromContext(r.Context(), nil)
				log.PrintErr("panic", def.LogHTTPStatus, code, "err", err, structlog.KeyStack, structlog.Auto)
				w.WriteHeader(code)
			case nil:
			case net.Error:
				log := structlog.FromContext(r.Context(), nil)
				log.PrintErr("recovered", def.LogHTTPStatus, code, "err", err)
				w.WriteHeader(code)
			}
		}()

		next.ServeHTTP(w, r)
	})
}

func makeAccessLog(basePath string) middlewareFunc {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			metric.reqInFlight.Inc()
			defer metric.reqInFlight.Dec()

			m := httpsnoop.CaptureMetrics(next, w, r)

			l := prometheus.Labels{
				resourceLabel: strings.TrimPrefix(r.URL.Path, basePath),
				methodLabel:   r.Method,
				codeLabel:     strconv.Itoa(m.Code),
			}
			if len(l[resourceLabel]) > 0 && l[resourceLabel][0] != '/' {
				l[resourceLabel] = "/" + l[resourceLabel]
			}

			metric.reqTotal.With(l).Inc()
			metric.reqDuration.With(l).Observe(m.Duration.Seconds())

			log := structlog.FromContext(r.Context(), nil)
			if m.Code >= 500 {
        log.PrintErr("failed to handle", "status", m.Code, "method", r.Method, "url", r.URL)
			}
		})
	}
}
