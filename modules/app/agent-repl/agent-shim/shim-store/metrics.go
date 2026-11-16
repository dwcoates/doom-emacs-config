package main

import (
	"context"
	"fmt"
	"time"

	"agentrepl/logging/causelog"
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
	return func(level, message string) {
		fields := logging.Fields{Operation: "store.metrics", Level: level}
		if level == "debug" {
			log.LogVerbose(fields, "%s", message)
			return
		}
		log.Log(fields, "%s", message)
	}
}

// peerMessagesName is how many ends of cross-session messages (SendMessage
// between Claude Code sessions) the store's delivery index holds, by `end`
// (sender, recipient). Read from the index at scrape time, so it is exact and
// durable across restarts; its differences over time are the messages sent
// and received.
const peerMessagesName = "agentrepl_peer_messages"

// peerMessagesTimeout bounds one scrape's count of the delivery index's ends.
// The count runs inside the store's scrape, so it shares every stall the
// scrape meets on a loaded host (the merge queue's full test runs), and its
// healthy worst case is the scrape's, not the statement's: the statement alone
// took at most 1.8ms in-process, even on a fresh read connection (300 runs,
// load average 9, 2026-11-16), yet a 25ms bound sized from it ran out three
// times in an hour of full test runs.
// MEASURED 2026-11-16 from the metrics store's scrape_duration_seconds for the
// store over 3 days and 12 runs: the whole scrape's 99.9th percentile was at
// most 266ms (two runs whose scrapes hung 23-31s, a suspended host, excluded).
// 650ms is 2.5 times that, well inside the scraper's 10s scrape_timeout.
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
			"the cross-session message count failed; the scrape omits %s until it answers timeout_ms=%d cause=%s",
			peerMessagesName, p.timeout.Milliseconds(), cause)
	}, func() {
		p.log.Log(logging.Fields{Operation: "store.metrics.peer-messages", Level: "info", Table: "peer_delivery"},
			"the cross-session message count answers again; the scrape serves %s", peerMessagesName)
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
		log.Log(logging.Fields{Operation: "store.metrics", Level: "error"}, "could not register %s: %v", peerMessagesName, err)
		return fmt.Errorf("register %s: %w", peerMessagesName, err)
	}
	return nil
}
