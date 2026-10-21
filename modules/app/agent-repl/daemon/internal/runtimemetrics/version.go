package runtimemetrics

import (
	"sync/atomic"

	"agent-repld/internal/deployedversion"

	"github.com/prometheus/client_golang/prometheus"
)

// DeployedVersionName is the series naming the deployed version, value 1.
const DeployedVersionName = "agentrepl_deployed_version_info"

// deployedVersion is the version the daemon last resolved for its checkout,
// exposed as DeployedVersionName (docs/METRICS-JUDGEMENT-CALLS.md, call 3).
// It is resolved off every hot path, by deployedversion.Loop, and only read
// here, so a scrape runs no git.
type deployedVersion struct {
	desc    *prometheus.Desc
	current atomic.Pointer[string]
}

func newDeployedVersion() *deployedVersion {
	return &deployedVersion{
		desc: prometheus.NewDesc(DeployedVersionName, "Always 1. The version label is the version of the checkout the daemon deploys from.", []string{"version"}, nil),
	}
}

func (d *deployedVersion) Describe(ch chan<- *prometheus.Desc) { ch <- d.desc }

func (d *deployedVersion) Collect(ch chan<- prometheus.Metric) {
	if v := d.current.Load(); v != nil {
		ch <- prometheus.MustNewConstMetric(d.desc, prometheus.GaugeValue, 1, *v)
	}
}

// DeployedVersion states v as the version series, past-commit metadata and
// all, so a build between landings is never attributed to the landing. Nil
// withdraws the series. The off surface states nothing.
func (s *Surface) DeployedVersion(v *deployedversion.Version) {
	if s == nil {
		return
	}
	if v == nil {
		s.version.current.Store(nil)
		return
	}
	text := v.String()
	s.version.current.Store(&text)
}
