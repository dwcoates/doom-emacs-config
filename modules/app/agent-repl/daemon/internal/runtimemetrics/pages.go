package runtimemetrics

import (
	"context"
	"fmt"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/sessionwhy"

	"agentrepl/logging/causelog"
)

// The page series: which pages Emacs's web view holds now (the frames the web
// host reports), why each was loaded (Emacs's report, or `unreported`) and
// what its workspace's session is doing, read from the held-page table
// (pageheld) when scraped; and every page load and unload Emacs reported, by
// reason.
const (
	// PagesLoadedName is how many pages the web view holds now, by the reason
	// each was loaded and its workspace's session state: the frames the web
	// host reports, each labelled with Emacs's reason or `unreported`.
	PagesLoadedName = "agentrepl_pages_loaded"
	// PageLoadsName counts every page load Emacs reported, by reason.
	PageLoadsName = "agentrepl_page_loads_total"
	// PageUnloadsName counts every page unload Emacs reported, by reason.
	PageUnloadsName = "agentrepl_page_unloads_total"
)

// The session states a loaded page's workspace is in (agentrepl_pages_loaded
// {session}).
const (
	// PageSessionWorking: a turn or detached work runs in its session.
	PageSessionWorking = "working"
	// PageSessionKeepalive: its session is up for a keep-alive.
	PageSessionKeepalive = "keepalive"
	// PageSessionIdle: its session is up for any other reason
	// (agentrepl_sessions_up says which).
	PageSessionIdle = "idle"
	// PageSessionParked: no shim is up, and its session record stands.
	PageSessionParked = "parked"
	// PageSessionNone: the workspace has no session on record.
	PageSessionNone = "no_session"
)

// pageTelemetry holds the load and unload counters, every reason's child
// resolved once so a count is one atomic add.
type pageTelemetry struct {
	loads, unloads *prometheus.CounterVec
	byLoad         closedVocab[pageheld.LoadReason, prometheus.Counter]
	byUnload       closedVocab[pageheld.UnloadReason, prometheus.Counter]
}

func newPageTelemetry() pageTelemetry {
	t := pageTelemetry{
		loads: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: PageLoadsName, Help: "Page loads Emacs reported, by the reason each was loaded.",
		}, []string{"reason"}),
		unloads: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: PageUnloadsName, Help: "Page unloads Emacs reported, by the reason each was dropped.",
		}, []string{"reason"}),
	}
	t.byLoad = newClosedVocab(PageLoadsName, pageheld.LoadReasons, func(r pageheld.LoadReason) prometheus.Counter {
		return t.loads.WithLabelValues(string(r))
	})
	t.byUnload = newClosedVocab(PageUnloadsName, pageheld.UnloadReasons, func(r pageheld.UnloadReason) prometheus.Counter {
		return t.unloads.WithLabelValues(string(r))
	})
	return t
}

// CountPageLoad implements pageheld.Telemetry. The off surface records
// nothing; a reason the vocabulary does not hold panics.
func (s *Surface) CountPageLoad(reason pageheld.LoadReason) {
	if s == nil {
		return
	}
	s.pages.byLoad.of(reason).Inc()
}

// CountPageUnload implements pageheld.Telemetry. The off surface records
// nothing; a reason the vocabulary does not hold panics.
func (s *Surface) CountPageUnload(reason pageheld.UnloadReason) {
	if s == nil {
		return
	}
	s.pages.byUnload.of(reason).Inc()
}

// SessionsRecorded answers every workspace with a session on record, in ONE
// read, for the loaded pages whose workspace holds no shim (parked or no
// session).
//
// A SCRAPE READS THE SESSION RECORDS AT ONCE. It used to read them one page
// at a time, and the state database has one connection: on 2026-11-25 11:36 a
// withheld stand-down's burst starved it, each page's read queued behind it in
// turn, and the scrape ran past stateReadBound.
type SessionsRecorded func(ctx context.Context) (map[ids.WorkspaceID]bool, error)

// pagesCollector serves agentrepl_pages_loaded from the held-page table and
// the sessions census at scrape.
type pagesCollector struct {
	pages    *pageheld.Table
	census   sessionwhy.Census
	recorded SessionsRecorded
	log      dlog.Logger
	causes   causelog.Tracker
	loaded   *prometheus.Desc
}

// Describe implements prometheus.Collector.
func (c *pagesCollector) Describe(ch chan<- *prometheus.Desc) { ch <- c.loaded }

// Collect implements prometheus.Collector. When the session records cannot be
// read, only the pages whose workspace holds no shim are omitted, recorded
// once per cause.
func (c *pagesCollector) Collect(ch chan<- prometheus.Metric) {
	up := c.census.Up()
	type key struct{ reason, session string }
	counts := map[key]float64{}
	unheld := map[ids.WorkspaceID]pageheld.LoadReason{}
	for ws, reason := range c.pages.Snapshot() {
		if state, held := heldSessionState(ws, up); held {
			counts[key{string(reason), state}]++
			continue
		}
		unheld[ws] = reason
	}
	readErr := c.countUnheld(unheld, func(reason pageheld.LoadReason, state string) {
		counts[key{string(reason), state}]++
	})
	c.causes.Note("page_sessions", readErr, func(cause string) {
		c.log.Error(Operation, "the loaded pages' session records could not be read; the pages whose workspace holds no shim are omitted from agentrepl_pages_loaded until they read", dlog.Context{"step": "page_sessions", "pages_omitted": len(unheld), "error": cause})
	}, func() {
		c.log.Info(Operation, "every loaded page's session reads again", dlog.Context{"step": "page_sessions"})
	})
	for k, n := range counts {
		ch <- prometheus.MustNewConstMetric(c.loaded, prometheus.GaugeValue, n, k.reason, k.session)
	}
}

// countUnheld counts each of UNHELD's pages under its session state, parked
// or no session, from one read of the session records; a scrape whose every
// page's workspace holds a shim reads nothing.
func (c *pagesCollector) countUnheld(unheld map[ids.WorkspaceID]pageheld.LoadReason, count func(pageheld.LoadReason, string)) error {
	if len(unheld) == 0 {
		return nil
	}
	ctx, cancel := context.WithTimeout(context.Background(), stateReadBound)
	defer cancel()
	recorded, err := c.recorded(ctx)
	if err != nil {
		return fmt.Errorf("read the session records of %d loaded pages: %w", len(unheld), err)
	}
	for ws, reason := range unheld {
		if recorded[ws] {
			count(reason, PageSessionParked)
		} else {
			count(reason, PageSessionNone)
		}
	}
	return nil
}

// heldSessionState answers the state of WS's session from the census's UP,
// and whether a shim is held for it at all.
func heldSessionState(ws ids.WorkspaceID, up map[ids.WorkspaceID]sessionwhy.Reason) (string, bool) {
	why, held := up[ws]
	if !held {
		return "", false
	}
	switch why {
	case sessionwhy.Working, sessionwhy.DetachedWork:
		return PageSessionWorking, true
	case sessionwhy.Keepalive:
		return PageSessionKeepalive, true
	default:
		return PageSessionIdle, true
	}
}

// RegisterPages registers the loaded-pages collector over PAGES, CENSUS and
// RECORDED. The off surface registers nothing.
func (s *Surface) RegisterPages(pages *pageheld.Table, census sessionwhy.Census, recorded SessionsRecorded) error {
	if s == nil {
		return nil
	}
	c := &pagesCollector{
		pages: pages, census: census, recorded: recorded, log: s.log,
		loaded: prometheus.NewDesc(PagesLoadedName, "Pages Emacs's web view holds now, as the web host reports its frames, by the reason Emacs gave for each load (or unreported) and its workspace's session state.", []string{"reason", "session"}, nil),
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the loaded-pages collector: %w", err)
	}
	return nil
}
