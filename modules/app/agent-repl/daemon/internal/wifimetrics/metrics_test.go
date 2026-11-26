package wifimetrics

import (
	"testing"

	"github.com/prometheus/client_golang/prometheus"
)

// served gathers reg and answers each sample's value, keyed by series name
// and the application label ("" for an unlabeled series).
func served(t *testing.T, reg *prometheus.Registry) map[[2]string]float64 {
	t.Helper()
	families, err := reg.Gather()
	if err != nil {
		t.Fatalf("Gather() error = %v", err)
	}
	out := map[[2]string]float64{}
	for _, f := range families {
		for _, m := range f.GetMetric() {
			app := ""
			for _, l := range m.GetLabel() {
				if l.GetName() == "app" {
					app = l.GetValue()
				}
			}
			if m.GetGauge() != nil {
				out[[2]string{f.GetName(), app}] = m.GetGauge().GetValue()
			} else {
				out[[2]string{f.GetName(), app}] = m.GetCounter().GetValue()
			}
		}
	}
	return out
}

func newRegistered(t *testing.T) (*Metrics, *prometheus.Registry) {
	t.Helper()
	m := New()
	reg := prometheus.NewRegistry()
	for _, c := range m.Collectors() {
		reg.MustRegister(c)
	}
	return m, reg
}

func TestMetricsServeTheQuietStatesSeries(t *testing.T) {
	// Arrange
	m, reg := newRegistered(t)

	// Act
	m.SetLidQuiet(true)
	m.CountAppFrozen("Slack")
	m.CountAppFrozen("Slack")
	m.CountAppThawed("Slack")
	got := served(t, reg)

	// Assert
	tests := []struct {
		name string
		key  [2]string
		want float64
	}{
		{"the lid gauge is 1 while the state stands", [2]string{LidQuietName, ""}, 1},
		{"each freeze is counted by application", [2]string{AppFreezesName, "Slack"}, 2},
		{"each thaw is counted by application", [2]string{AppThawsName, "Slack"}, 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got[tt.key] != tt.want {
				t.Fatalf("%v = %v, want %v (all: %v)", tt.key, got[tt.key], tt.want, got)
			}
		})
	}
}

func TestTheLidGaugeFallsBackToZero(t *testing.T) {
	// Arrange
	m, reg := newRegistered(t)
	m.SetLidQuiet(true)

	// Act
	m.SetLidQuiet(false)

	// Assert
	if got := served(t, reg)[[2]string{LidQuietName, ""}]; got != 0 {
		t.Fatalf("gauge = %v, want 0", got)
	}
}
