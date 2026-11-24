package runtimemetrics

import (
	"fmt"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// The visibility series: how many workspaces stand in each visibility and
// why, every visibility change, and every last turn result made read
// (sidebar.VisibilityTelemetry). The roster derives each workspace's
// visibility and its reason in one derivation (wsm.VisibilityReasons), so
// the workspaces counted by reason add up to those counted by visibility.
const (
	// WorkspacesVisibleName is how many registry workspaces stand under each
	// visibility and reason now.
	WorkspacesVisibleName = "agentrepl_workspaces_visible"
	// VisibilityChangesName counts every visibility change the roster
	// derived, by the visibility it left, the one it took, and its reason.
	VisibilityChangesName = "agentrepl_visibility_changes_total"
	// ResultsReadName counts every last turn result made read, by what read
	// it.
	ResultsReadName = "agentrepl_workspace_results_read_total"
)

// visibilityTelemetry holds the visibility series. Every reason's gauge and
// every reader's counter is resolved once here, so a render's update is an
// atomic set on a map nothing writes after construction.
type visibilityTelemetry struct {
	visible  *prometheus.GaugeVec
	changes  *prometheus.CounterVec
	read     *prometheus.CounterVec
	byReason map[wsm.VisibilityReason]prometheus.Gauge
	byReader map[sidebar.ResultReadBy]prometheus.Counter
}

func newVisibilityTelemetry() visibilityTelemetry {
	t := visibilityTelemetry{
		visible: prometheus.NewGaugeVec(prometheus.GaugeOpts{
			Name: WorkspacesVisibleName, Help: "Registry workspaces by visibility and the reason the roster derived it.",
		}, []string{"visibility", "reason"}),
		changes: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: VisibilityChangesName, Help: "Visibility changes the roster derived, by the visibility left, the one taken, and its reason.",
		}, []string{"from", "to", "reason"}),
		read: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ResultsReadName, Help: "Last turn results made read, by what read them.",
		}, []string{"by"}),
		byReason: map[wsm.VisibilityReason]prometheus.Gauge{},
		byReader: map[sidebar.ResultReadBy]prometheus.Counter{},
	}
	for _, r := range wsm.VisibilityReasonsAll {
		t.byReason[r] = t.visible.WithLabelValues(r.Visibility().String(), r.String())
	}
	for _, by := range sidebar.ResultReadersAll {
		t.byReader[by] = t.read.WithLabelValues(string(by))
	}
	return t
}

// SetVisibilityCounts implements sidebar.VisibilityTelemetry. The off
// surface records nothing. A reason the vocabulary does not hold is a broken
// invariant and panics.
func (s *Surface) SetVisibilityCounts(counts map[wsm.VisibilityReason]int) {
	if s == nil {
		return
	}
	for r, n := range counts {
		g, ok := s.visibility.byReason[r]
		if !ok {
			panic(fmt.Sprintf("runtimemetrics: a visibility count names reason %d, which wsm.VisibilityReasonsAll does not hold", int(r)))
		}
		g.Set(float64(n))
	}
}

// CountVisibilityChange implements sidebar.VisibilityTelemetry. The off
// surface records nothing.
func (s *Surface) CountVisibilityChange(from, to wsm.Visibility, reason wsm.VisibilityReason) {
	if s == nil {
		return
	}
	s.visibility.changes.WithLabelValues(from.String(), to.String(), reason.String()).Inc()
}

// CountResultRead implements sidebar.VisibilityTelemetry. The off surface
// records nothing. A reader the vocabulary does not hold is a broken
// invariant and panics.
func (s *Surface) CountResultRead(by sidebar.ResultReadBy) {
	if s == nil {
		return
	}
	c, ok := s.visibility.byReader[by]
	if !ok {
		panic(fmt.Sprintf("runtimemetrics: a result read names reader %q, which sidebar.ResultReadersAll does not hold", by))
	}
	c.Inc()
}
