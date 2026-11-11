package runtimemetrics

import "github.com/prometheus/client_golang/prometheus"

// The lid-closed quiet state's series (persistentwifi, appfreeze, viewquiet).
const (
	// LidQuietName is 1 while persistent wifi's lid-closed quiet state
	// stands, 0 otherwise.
	LidQuietName = "agentrepl_lid_quiet"
	// AppFreezesName counts each application the quiet state froze, by
	// application.
	AppFreezesName = "agentrepl_app_freezes_total"
	// AppThawsName counts each application the quiet state thawed, by
	// application.
	AppThawsName = "agentrepl_app_thaws_total"
	// ViewRendersSkippedName counts the view renders and publications the
	// quiet state skipped, by view (sidebar, footer, topbar, feed).
	ViewRendersSkippedName = "agentrepl_view_renders_skipped_total"
)

// quietTelemetry holds the quiet state's series; each update is
// client_golang's own atomic one.
type quietTelemetry struct {
	quiet          prometheus.Gauge
	freezes, thaws *prometheus.CounterVec
	skipped        *prometheus.CounterVec
}

func newQuietTelemetry() quietTelemetry {
	return quietTelemetry{
		quiet: prometheus.NewGauge(prometheus.GaugeOpts{
			Name: LidQuietName, Help: "1 while persistent wifi's lid-closed quiet state stands.",
		}),
		freezes: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: AppFreezesName, Help: "Applications the lid-closed quiet state froze, by application.",
		}, []string{"app"}),
		thaws: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: AppThawsName, Help: "Applications the lid-closed quiet state thawed, by application.",
		}, []string{"app"}),
		skipped: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ViewRendersSkippedName, Help: "View renders the lid-closed quiet state skipped, by view.",
		}, []string{"view"}),
	}
}

// SetLidQuiet implements persistentwifi.QuietTelemetry. The off surface
// records nothing.
func (s *Surface) SetLidQuiet(quiet bool) {
	if s == nil {
		return
	}
	v := 0.0
	if quiet {
		v = 1
	}
	s.quiet.quiet.Set(v)
}

// CountAppFrozen implements appfreeze.Telemetry. The off surface records
// nothing.
func (s *Surface) CountAppFrozen(app string) {
	if s == nil {
		return
	}
	s.quiet.freezes.WithLabelValues(app).Inc()
}

// CountAppThawed implements appfreeze.Telemetry. The off surface records
// nothing.
func (s *Surface) CountAppThawed(app string) {
	if s == nil {
		return
	}
	s.quiet.thaws.WithLabelValues(app).Inc()
}

// CountViewRenderSkipped implements viewquiet.Telemetry. The off surface
// records nothing.
func (s *Surface) CountViewRenderSkipped(view string) {
	if s == nil {
		return
	}
	s.quiet.skipped.WithLabelValues(view).Inc()
}
