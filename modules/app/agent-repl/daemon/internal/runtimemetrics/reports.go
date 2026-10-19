package runtimemetrics

import (
	"context"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// ValidateReportedLatency checks one latency a client reported, answering
// the kind it names. Every error wraps ErrInvalidLatency.
func ValidateReportedLatency(l *agentreplv1.ReportedLatency) (LatencyKind, error) {
	return validateReported(l)
}

// Enabled reports whether the surface records anything.
func (s *Surface) Enabled() bool { return s != nil }

// ObserveLatency records one latency a client reported for ws, already
// validated. The off surface records nothing.
func (s *Surface) ObserveLatency(ws ids.WorkspaceID, kind LatencyKind, seconds float64) {
	if s == nil {
		return
	}
	s.latencies.observe(ws, kind, seconds)
}

// FoldShimMeasurements folds one shim measurements frame for ws. The off
// surface folds nothing.
func (s *Surface) FoldShimMeasurements(ws ids.WorkspaceID, m *shimv1.ShimMeasurements) {
	if s == nil {
		return
	}
	s.latencies.foldShim(ws, m)
}

// ForgetWorkspace deletes every series of ws: it left the registry.
func (s *Surface) ForgetWorkspace(ws ids.WorkspaceID) {
	if s == nil {
		return
	}
	s.latencies.forget(ws)
	s.restarts.forget(ws)
}

// ObserveForgets answers db with Forget also deleting the workspace's
// series once the registry forgot it. Every way a workspace leaves the
// registry goes through wsm.DB.Forget, so wrapping the one client the daemon
// hands its components makes a forgotten workspace's series unable to
// outlive it. The off surface answers db itself.
func (s *Surface) ObserveForgets(db wsm.DB) wsm.DB {
	if s == nil {
		return db
	}
	return forgetObservingDB{DB: db, surface: s}
}

// forgetObservingDB is a wsm.DB whose Forget deletes the workspace's series
// after the registry forgot it.
type forgetObservingDB struct {
	wsm.DB
	surface *Surface
}

// Forget implements wsm.DB.
func (d forgetObservingDB) Forget(ctx context.Context, id wsm.WorkspaceID) (wsm.ForgetReport, error) {
	report, err := d.DB.Forget(ctx, id)
	if err == nil {
		d.surface.ForgetWorkspace(id)
	}
	return report, err
}
