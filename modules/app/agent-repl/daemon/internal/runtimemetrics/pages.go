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

// The page series: which pages Emacs's web view holds now, why each was
// loaded and what its workspace's session is doing, read from the held-page
// table (pageheld) when scraped; and every page load and unload Emacs
// reported, by reason.
const (
	// PagesLoadedName is how many pages the web view holds now, by the reason
	// each was loaded and its workspace's session state.
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

// SessionRecorded answers whether WS has a session on record, read for a
// loaded page whose workspace holds no shim (parked or no session).
type SessionRecorded func(ctx context.Context, ws ids.WorkspaceID) (bool, error)

// pagesCollector serves agentrepl_pages_loaded from the held-page table and
// the sessions census at scrape.
type pagesCollector struct {
	pages    *pageheld.Table
	census   sessionwhy.Census
	recorded SessionRecorded
	log      dlog.Logger
	causes   causelog.Tracker
	loaded   *prometheus.Desc
}

// Describe implements prometheus.Collector.
func (c *pagesCollector) Describe(ch chan<- *prometheus.Desc) { ch <- c.loaded }

// Collect implements prometheus.Collector. A session record that cannot be
// read omits only that page, recorded once per cause.
func (c *pagesCollector) Collect(ch chan<- prometheus.Metric) {
	ctx, cancel := context.WithTimeout(context.Background(), stateReadBound)
	defer cancel()
	up := c.census.Up()
	type key struct{ reason, session string }
	counts := map[key]float64{}
	var readErr error
	for ws, reason := range c.pages.Snapshot() {
		state, err := c.sessionState(ctx, ws, up)
		if err != nil {
			readErr = fmt.Errorf("read the session record of %s: %w", ws, err)
			continue
		}
		counts[key{string(reason), state}]++
	}
	c.causes.Note("page_sessions", readErr, func(cause string) {
		c.log.Error(Operation, "a loaded page's session could not be read; that page is omitted from agentrepl_pages_loaded until it reads", dlog.Context{"step": "page_sessions", "error": cause})
	}, func() {
		c.log.Info(Operation, "every loaded page's session reads again", dlog.Context{"step": "page_sessions"})
	})
	for k, n := range counts {
		ch <- prometheus.MustNewConstMetric(c.loaded, prometheus.GaugeValue, n, k.reason, k.session)
	}
}

// sessionState answers the state of WS's session, from the census's UP when
// a shim is held, else from its session record.
func (c *pagesCollector) sessionState(ctx context.Context, ws ids.WorkspaceID, up map[ids.WorkspaceID]sessionwhy.Reason) (string, error) {
	if why, held := up[ws]; held {
		switch why {
		case sessionwhy.Working, sessionwhy.DetachedWork:
			return PageSessionWorking, nil
		case sessionwhy.Keepalive:
			return PageSessionKeepalive, nil
		default:
			return PageSessionIdle, nil
		}
	}
	recorded, err := c.recorded(ctx, ws)
	if err != nil {
		return "", err
	}
	if recorded {
		return PageSessionParked, nil
	}
	return PageSessionNone, nil
}

// RegisterPages registers the loaded-pages collector over PAGES, CENSUS and
// RECORDED. The off surface registers nothing.
func (s *Surface) RegisterPages(pages *pageheld.Table, census sessionwhy.Census, recorded SessionRecorded) error {
	if s == nil {
		return nil
	}
	c := &pagesCollector{
		pages: pages, census: census, recorded: recorded, log: s.log,
		loaded: prometheus.NewDesc(PagesLoadedName, "Pages Emacs's web view holds now, by the reason each was loaded and its workspace's session state.", []string{"reason", "session"}, nil),
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the loaded-pages collector: %w", err)
	}
	return nil
}
