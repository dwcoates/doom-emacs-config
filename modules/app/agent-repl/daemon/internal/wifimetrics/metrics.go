// Package wifimetrics holds the persistent-wifi service's own series. It is
// its own package so the persistentwifi package, which other modules import
// for its constants, carries no metrics dependency.
package wifimetrics

import "github.com/prometheus/client_golang/prometheus"

// The series the lid-closed quiet state exposes on the persistent-wifi
// service's metrics surface (metrics.ProgramPersistentWifi).
const (
	// LidQuietName is 1 while the lid-closed quiet state stands, 0 otherwise.
	LidQuietName = "agentrepl_lid_quiet"
	// AppFreezesName counts each application the quiet state froze, by
	// application.
	AppFreezesName = "agentrepl_app_freezes_total"
	// AppThawsName counts each application the quiet state thawed, by
	// application.
	AppThawsName = "agentrepl_app_thaws_total"
)

// Metrics holds the quiet state's series. It is the controller's
// persistentwifi.QuietTelemetry and the freezer's appfreeze.Telemetry, and its collectors
// register on the service's own registry (Collectors). Each update is
// client_golang's own atomic one.
type Metrics struct {
	quiet          prometheus.Gauge
	freezes, thaws *prometheus.CounterVec
}

// New builds the quiet state's series, none registered yet.
func New() *Metrics {
	return &Metrics{
		quiet: prometheus.NewGauge(prometheus.GaugeOpts{
			Name: LidQuietName, Help: "1 while persistent wifi's lid-closed quiet state stands.",
		}),
		freezes: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: AppFreezesName, Help: "Applications the lid-closed quiet state froze, by application.",
		}, []string{"app"}),
		thaws: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: AppThawsName, Help: "Applications the lid-closed quiet state thawed, by application.",
		}, []string{"app"}),
	}
}

// Collectors are the series to register on the service's registry.
func (m *Metrics) Collectors() []prometheus.Collector {
	return []prometheus.Collector{m.quiet, m.freezes, m.thaws}
}

// SetLidQuiet implements QuietTelemetry.
func (m *Metrics) SetLidQuiet(quiet bool) {
	v := 0.0
	if quiet {
		v = 1
	}
	m.quiet.Set(v)
}

// CountAppFrozen implements appfreeze.Telemetry.
func (m *Metrics) CountAppFrozen(app string) { m.freezes.WithLabelValues(app).Inc() }

// CountAppThawed implements appfreeze.Telemetry.
func (m *Metrics) CountAppThawed(app string) { m.thaws.WithLabelValues(app).Inc() }
