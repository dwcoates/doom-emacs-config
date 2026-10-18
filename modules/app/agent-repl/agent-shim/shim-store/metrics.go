package main

import (
	"agentrepl/logging/metrics"
	"agentrepl/shim-store/internal/logging"
)

// openMetricsSurface opens and advertises the store's ALWAYS-ON metrics
// surface under root (docs/PLAN-runtime-metrics.md) and records the decision
// either way. Off (no root) is recorded at debug; a configured surface that
// cannot open is a boot error, because a store the launcher asked to measure
// and that silently is not would leave a hole nobody sees in the history.
func openMetricsSurface(root string, log *logging.Logger) (*metrics.Server, error) {
	surface, err := metrics.OpenSelf(metrics.ProgramStore, root, func(op string, err error) {
		log.Log(logging.Fields{Operation: "store.metrics", Level: "error"}, "the metrics surface failed step=%s: %v", op, err)
	})
	if err != nil {
		log.Log(logging.Fields{Operation: "store.metrics", Level: "error"}, "the metrics surface could not open root=%s: %v", root, err)
		return nil, err
	}
	if surface == nil {
		log.LogVerbose(logging.Fields{Operation: "store.metrics", Level: "debug"}, "the metrics surface is off flag=--metrics-root")
		return nil, nil
	}
	log.Log(logging.Fields{Operation: "store.metrics", Level: "info"},
		"the metrics surface is listening address=%s target=%s", surface.Addr(), surface.TargetPath())
	return surface, nil
}

// closeMetricsSurface stops an open surface, recording a failure to stop.
func closeMetricsSurface(surface *metrics.Server, log *logging.Logger) {
	if surface == nil {
		return
	}
	if err := surface.Close(); err != nil {
		log.Log(logging.Fields{Operation: "store.metrics", Level: "error"}, "the metrics surface did not stop cleanly: %v", err)
	}
}
