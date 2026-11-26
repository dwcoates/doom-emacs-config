package runtimemetrics

import "github.com/prometheus/client_golang/prometheus"

// The series of the views the lid-closed quiet state stops. The quiet state's
// own series (the lid gauge and the application freeze and thaw counters)
// belong to the persistent-wifi service (persistentwifi.NewMetrics).
const (
	// ViewRendersSkippedName counts the view renders and publications the
	// quiet state skipped, by view (sidebar, footer, topbar, feed).
	ViewRendersSkippedName = "agentrepl_view_renders_skipped_total"
)

// quietTelemetry holds the views' series; each update is client_golang's own
// atomic one.
type quietTelemetry struct {
	skipped *prometheus.CounterVec
}

func newQuietTelemetry() quietTelemetry {
	return quietTelemetry{
		skipped: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ViewRendersSkippedName, Help: "View renders the lid-closed quiet state skipped, by view.",
		}, []string{"view"}),
	}
}

// CountViewRenderSkipped implements viewquiet.Telemetry. The off surface
// records nothing.
func (s *Surface) CountViewRenderSkipped(view string) {
	if s == nil {
		return
	}
	s.quiet.skipped.WithLabelValues(view).Inc()
}
