package main

import (
	"agentrepl/logging/metrics"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// openMetricsSurface opens and advertises the sidecar's ALWAYS-ON metrics
// surface under root (docs/PLAN-runtime-metrics.md), every decision recorded
// as metrics (metrics.OpenService). A configured surface that cannot open is
// a boot error.
func openMetricsSurface(root string, logf *logging.Bound) (*metrics.Server, error) {
	return metrics.OpenService(metrics.ProgramSidecar, root, metricsRecord(logf))
}

// closeMetricsSurface stops an open surface, recording a failure to stop.
func closeMetricsSurface(surface *metrics.Server, logf *logging.Bound) {
	metrics.CloseService(surface, metricsRecord(logf))
}

// metricsRecord is the sidecar's canonical logger as the metrics surface
// records through it; a debug record is verbose.
func metricsRecord(logf *logging.Bound) metrics.Record {
	return func(level, message string) {
		bound := logf.With(logging.Context{Operation: "metrics", Level: level})
		if level == "debug" {
			bound.LogVerbose("%s", message)
			return
		}
		bound.Log("%s", message)
	}
}
