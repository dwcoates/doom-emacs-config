package main

import (
	"context"
	"fmt"
	"time"

	"agentrepl/logging/causelog"
	"agentrepl/logging/logsite"
	"agentrepl/logging/metrics"
	"agentrepl/shim-store/internal/logging"

	"github.com/prometheus/client_golang/prometheus"
)

// openMetricsSurface opens and advertises the store's ALWAYS-ON metrics
// surface under root (docs/PLAN-runtime-metrics.md), every decision recorded
// as store.metrics (metrics.OpenService). A configured surface that cannot
// open is a boot error. It serves the sizes of the database at dbPath
// (metrics.NewDBFiles, db="events").
func openMetricsSurface(root, dbPath string, log *logging.Logger) (*metrics.Server, error) {
	return metrics.OpenService(metrics.ProgramStore, root, metricsRecord(log), metrics.NewDBFiles("events", dbPath))
}

// closeMetricsSurface stops an open surface, recording a failure to stop.
func closeMetricsSurface(surface *metrics.Server, log *logging.Logger) {
	metrics.CloseService(surface, metricsRecord(log))
}

// metricsRecord is the store's canonical logger as the metrics surface
// records through it; a debug record is verbose.
func metricsRecord(log *logging.Logger) metrics.Record {
	return func(level string, message logsite.Message, values map[string]any) {
		fields := logging.Fields{Operation: "store.metrics", Level: level}
		if level == "debug" {
			log.LogVerboseMessage(fields, message, logging.Values(values))
			return
		}
		log.LogMessage(fields, message, logging.Values(values))
	}
}

// peerMessagesName is how many ends of cross-session messages (SendMessage
// between Claude Code sessions) the store's delivery index holds, by `end`
// (sender, recipient). Read from the index at scrape time, so it is exact and
// durable across restarts; its differences over time are the messages sent
// and received.
const peerMessagesName = "agentrepl_peer_messages"

// peerMessagesMeasured is the healthy worst case of one count on a loaded
// host. The count runs inside the store's scrape, on the store's own read
// pool, so it shares every stall the scrape meets while the merge queue's
// full test runs load the host; a standalone sqlite3 process (at most 10ms,
// 2026-11-15) leaves all of that out, and its 25ms bound ran out five times
// on 2026-11-16. MEASURED 2026-11-16, the larger of:
//   - the metrics store's scrape_duration_seconds for the store over 3 days
//     and 12 runs: the whole scrape's 99.9th percentile was at most 266ms
//     (two runs whose scrapes hung 23-31s, a suspended host, excluded);
//   - the live store's store.db.slow-query records from 2026-10-20 to
//     2026-11-16, timed from before the read pool hands out a connection:
//     peer_delivery, the delivery index's other read over the same tables
//     and join, took at most 288ms.
//
// The statement alone, in-process against a copy of the store at load
// average 17-23, took at most 4.1ms on a pooled connection and 59ms when the
// pool opened a fresh one (1,800 counts per mode).
const peerMessagesMeasured = 288 * time.Millisecond

// peerMessagesTimeout bounds one scrape's count of the delivery index's ends:
// 2.26 times peerMessagesMeasured, and well inside the scraper's 10s
// scrape_timeout (metrics/scrape.yml), so a slow count never costs the scrape
// the store's other series. A count over the store's slow-query budget is
// recorded as store.db.slow-query, which is how a creeping count shows before
// it reaches this bound.
const peerMessagesTimeout = 650 * time.Millisecond

// peerMessageEnds is the store's count of each end of the cross-session
// messages it indexed (db.DB.PeerMessageEnds).
type peerMessageEnds interface {
	PeerMessageEnds(ctx context.Context) (map[string]int64, error)
}

// peerMessages is the collector of agentrepl_peer_messages. A count that
// fails or runs out its bound omits only this series from the scrape, and is
// recorded once per cause through the store's logger: the scrape after it
// that fails the same way records nothing, and the first that answers again
// records the recovery.
type peerMessages struct {
	ends    peerMessageEnds
	log     *logging.Logger
	timeout time.Duration
	desc    *prometheus.Desc
	causes  causelog.Tracker
}

func newPeerMessages(ends peerMessageEnds, log *logging.Logger, timeout time.Duration) *peerMessages {
	return &peerMessages{ends: ends, log: log, timeout: timeout, desc: prometheus.NewDesc(peerMessagesName,
		"Ends of cross-session messages the store's delivery index holds, by end.", []string{"end"}, nil)}
}

// Describe implements prometheus.Collector.
func (p *peerMessages) Describe(ch chan<- *prometheus.Desc) { ch <- p.desc }

// Collect implements prometheus.Collector.
func (p *peerMessages) Collect(ch chan<- prometheus.Metric) {
	ctx, cancel := context.WithTimeout(context.Background(), p.timeout)
	defer cancel()
	ends, err := p.ends.PeerMessageEnds(ctx)
	answered := p.causes.Note("count", err, func(cause string) {
		p.log.Log(logging.Fields{Operation: "store.metrics.peer-messages", Level: "error", Table: "peer_delivery"},
			"the cross-session message count failed; the scrape omits {metric} until it answers timeout_ms={timeout_ms} expected_ms={expected_ms} cause={cause}",
			logging.Values{"metric": peerMessagesName, "timeout_ms": p.timeout.Milliseconds(), "expected_ms": peerMessagesMeasured.Milliseconds(), "cause": cause})
	}, func() {
		p.log.Log(logging.Fields{Operation: "store.metrics.peer-messages", Level: "info", Table: "peer_delivery"},
			"the cross-session message count answers again; the scrape serves {metric}", logging.Values{"metric": peerMessagesName})
	})
	if !answered {
		return
	}
	for end, count := range ends {
		ch <- prometheus.MustNewConstMetric(p.desc, prometheus.GaugeValue, float64(count), end)
	}
}

// registerPeerMessages serves agentrepl_peer_messages on an open surface from
// the opened database. The off surface (nil) serves nothing.
func registerPeerMessages(surface *metrics.Server, ends peerMessageEnds, log *logging.Logger) error {
	if surface == nil {
		return nil
	}
	if err := surface.Registry().Register(newPeerMessages(ends, log, peerMessagesTimeout)); err != nil {
		log.Log(logging.Fields{Operation: "store.metrics", Level: "error"}, "could not register {metric}: {cause}", logging.Values{"metric": peerMessagesName, "cause": err.Error()})
		return fmt.Errorf("register %s: %w", peerMessagesName, err)
	}
	return nil
}
