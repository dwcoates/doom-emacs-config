package runtimemetrics

import (
	"context"
	"fmt"
	"math"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/wsm"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// ValidateReportedLatency checks one latency a client reported, answering
// the kind it names. Every error wraps ErrInvalidLatency.
func ValidateReportedLatency(l *agentreplv1.ReportedLatency) (LatencyKind, error) {
	return validateReported(l)
}

// ValidateEditorLatency checks one editor-wide latency Emacs reported,
// answering the kind it names. Every error wraps ErrInvalidLatency.
func ValidateEditorLatency(l *agentreplv1.ReportedEditorLatency) (LatencyKind, error) {
	return validateEditor(l)
}

// editorMilestones maps each reported editor startup milestone to its phase.
var editorMilestones = map[agentreplv1.EditorStartupMilestone]restarttiming.EditorMilestone{
	agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_INIT:          restarttiming.EditorInit,
	agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_DAEMON_LINKED: restarttiming.EditorDaemonLinked,
	agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_FIRST_TAB:     restarttiming.EditorFirstTab,
	agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_ALL_TABS:      restarttiming.EditorAllTabs,
}

// ValidateEditorStartup checks one reported editor startup milestone and
// answers the restart observation it records: a known milestone, a daemon arm
// set, and a finite, non-negative time since the editor's process start.
func ValidateEditorStartup(m *agentreplv1.ReportedEditorStartup) (restarttiming.Observation, error) {
	milestone, ok := editorMilestones[m.GetMilestone()]
	if !ok {
		return restarttiming.Observation{}, fmt.Errorf("%w: milestone %v is not an editor startup milestone", ErrInvalidLatency, m.GetMilestone())
	}
	var daemon restarttiming.EditorDaemon
	switch m.GetDaemon().(type) {
	case *agentreplv1.ReportedEditorStartup_Spawned:
		daemon = restarttiming.EditorSpawned
	case *agentreplv1.ReportedEditorStartup_Running:
		daemon = restarttiming.EditorRunning
	default:
		return restarttiming.Observation{}, fmt.Errorf("%w: milestone %s names no way the editor came to a daemon", ErrInvalidLatency, milestone)
	}
	s := m.GetSecondsSinceProcessStart()
	if math.IsNaN(s) || math.IsInf(s, 0) || s < 0 {
		return restarttiming.Observation{}, fmt.Errorf("%w: milestone %s came %v seconds after the process start", ErrInvalidLatency, milestone, s)
	}
	return restarttiming.OfEditorStart(milestone, daemon, time.Duration(s*float64(time.Second))), nil
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

// ObserveEditorLatency records one editor-wide latency, already validated,
// under the empty workspace: it belongs to the editor, not to any workspace,
// so no workspace's close forgets it. The off surface records nothing.
func (s *Surface) ObserveEditorLatency(kind LatencyKind, seconds float64) {
	if s == nil {
		return
	}
	s.latencies.observe("", kind, seconds)
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
