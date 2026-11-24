package runtimemetrics

import (
	"fmt"
	"time"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimlife"
)

// The session series: why each session this daemon holds up is up, read
// from the census when scraped, and how long each parked session stood up
// idle before its park.
const (
	// SessionsUpName is 1 for every session up now, by why it is up
	// (sessionwhy.Reason) and its workspace; summed by reason it is how many
	// sessions each reason holds up.
	SessionsUpName = "agentrepl_sessions_up"
	// IdleBeforeParkName is how long each session the drain parked stood up
	// idle, from the later of its last real turn's end and its own start, by
	// the stand-down reason it was parked for (shimlife.StandDownReason).
	IdleBeforeParkName = "agentrepl_session_idle_before_park_seconds"
)

// idleBeforeParkBuckets span a cold park taken at once to a session up a
// day: the grace (20 minutes, cachelapse.StandDownGrace) and the hour around
// it are each a bucket edge, so "parked 20 minutes after its work" reads off
// the 1200 and 1500 buckets.
var idleBeforeParkBuckets = []float64{10, 60, 300, 600, 1200, 1500, 1800, 2700, 3600, 7200, 14400, 28800, 86400}

func newIdleBeforePark() *prometheus.HistogramVec {
	return prometheus.NewHistogramVec(prometheus.HistogramOpts{
		Name: IdleBeforeParkName, Help: "How long each parked session stood up idle before its park, by the reason it was parked for.",
		Buckets: idleBeforeParkBuckets,
	}, []string{"reason"})
}

// ObserveIdleBeforePark implements drain.ParkTelemetry. The off surface
// records nothing.
func (s *Surface) ObserveIdleBeforePark(reason shimlife.StandDownReason, idle time.Duration) {
	if s == nil {
		return
	}
	s.idleBeforePark.WithLabelValues(string(reason)).Observe(idle.Seconds())
}

// sessionsCollector serves agentrepl_sessions_up from the census at scrape.
type sessionsCollector struct {
	census sessionwhy.Census
	up     *prometheus.Desc
}

// Describe implements prometheus.Collector.
func (c *sessionsCollector) Describe(ch chan<- *prometheus.Desc) { ch <- c.up }

// Collect implements prometheus.Collector.
func (c *sessionsCollector) Collect(ch chan<- prometheus.Metric) {
	for ws, reason := range c.census.Up() {
		ch <- prometheus.MustNewConstMetric(c.up, prometheus.GaugeValue, 1, string(reason), string(ws))
	}
}

// RegisterSessions registers the sessions-up collector over CENSUS. The off
// surface registers nothing.
func (s *Surface) RegisterSessions(census sessionwhy.Census) error {
	if s == nil {
		return nil
	}
	c := &sessionsCollector{
		census: census,
		up:     prometheus.NewDesc(SessionsUpName, "Sessions up now, 1 each, by why each is up and its workspace.", []string{"reason", "workspace"}, nil),
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the sessions-up collector: %w", err)
	}
	return nil
}
