package main

import (
	"agentrepl/logging/metrics"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// openMetricsSurface opens and advertises the sidecar's ALWAYS-ON metrics
// surface under root (docs/PLAN-runtime-metrics.md) and records the decision
// either way. Off (no root) is recorded at debug; a configured surface that
// cannot open is a boot error, because a sidecar the launcher asked to
// measure and that silently is not would leave a hole nobody sees in the
// history.
func openMetricsSurface(root string, logf *logging.Bound) (*metrics.Server, error) {
	surface, err := metrics.OpenSelf(metrics.ProgramSidecar, root, func(op string, err error) {
		logf.With(logging.Context{Operation: "metrics", Level: "error"}).Log("the metrics surface failed step=%s: %v", op, err)
	})
	if err != nil {
		logf.With(logging.Context{Operation: "metrics", Level: "error"}).Log("the metrics surface could not open root=%s: %v", root, err)
		return nil, err
	}
	if surface == nil {
		logf.With(logging.Context{Operation: "metrics", Level: "debug"}).LogVerbose("the metrics surface is off flag=--metrics-root")
		return nil, nil
	}
	logf.With(logging.Context{Operation: "metrics", Level: "info"}).Log(
		"the metrics surface is listening address=%s target=%s", surface.Addr(), surface.TargetPath())
	return surface, nil
}

// closeMetricsSurface stops an open surface, recording a failure to stop.
func closeMetricsSurface(surface *metrics.Server, logf *logging.Bound) {
	if surface == nil {
		return
	}
	if err := surface.Close(); err != nil {
		logf.With(logging.Context{Operation: "metrics", Level: "error"}).Log("the metrics surface did not stop cleanly: %v", err)
	}
}
