package main

import (
	"agentrepl/logging/metrics"
	"agentrepl/shim-store/internal/logging"
)

// openMetricsSurface opens and advertises the store's ALWAYS-ON metrics
// surface under root (docs/PLAN-runtime-metrics.md), every decision recorded
// as store.metrics (metrics.OpenService). A configured surface that cannot
// open is a boot error. It serves the sizes of the database at dbPath
// (metrics.NewDBFiles, db="events").
func openMetricsSurface(root, dbPath string, log *logging.Logger) (*metrics.Server, error) {
	return metrics.OpenService(metrics.ProgramStore, root, metricsRecord(log), metrics.NewDBFiles("events", dbPath))
}

// closeMetricsSurface stops an open surface, recording a failure to stop.
func closeMetricsSurface(surface *metrics.Server, log *logging.Logger) {
	metrics.CloseService(surface, metricsRecord(log))
}

// metricsRecord is the store's canonical logger as the metrics surface
// records through it; a debug record is verbose.
func metricsRecord(log *logging.Logger) metrics.Record {
	return func(level, message string) {
		fields := logging.Fields{Operation: "store.metrics", Level: level}
		if level == "debug" {
			log.LogVerbose(fields, "%s", message)
			return
		}
		log.Log(fields, "%s", message)
	}
}
