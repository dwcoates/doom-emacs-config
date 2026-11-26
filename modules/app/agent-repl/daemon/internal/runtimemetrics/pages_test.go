package runtimemetrics

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/sessionwhy"
)

// pagesCensus answers a census holding A working, K up for a keep-alive and
// I up in view.
func pagesCensus() sessionwhy.Census {
	table := sessionwhy.NewTable()
	table.SetOwner("k", sessionwhy.OwnerCycle)
	table.SetOwner("i", sessionwhy.OwnerInView)
	return sessionwhy.Census{
		Live: func() []ids.WorkspaceID { return []ids.WorkspaceID{"a", "k", "i"} },
		Work: func(ws ids.WorkspaceID) (sessionwhy.Work, bool) {
			return sessionwhy.Work{Turn: ws == "a"}, true
		},
		Table: table,
		Now:   func() time.Time { return time.Unix(1_700_000_000, 0) },
	}
}

func TestLoadedPagesAreServedByReasonAndSessionState(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAllocated)
	pages.Held("k", pageheld.LoadSlot)
	pages.Held("i", pageheld.LoadShown)
	pages.Held("p", pageheld.LoadShown)
	pages.Held("n", pageheld.LoadSlot)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"a", "k", "i", "p", "n"}, false)
	recorded := func(context.Context) (map[ids.WorkspaceID]bool, error) {
		return map[ids.WorkspaceID]bool{"p": true}, nil
	}
	if err := s.RegisterPages(pages, pagesCensus(), recorded); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="allocated",session="working"} 1`,
		`agentrepl_pages_loaded{reason="slot",session="keepalive"} 1`,
		`agentrepl_pages_loaded{reason="shown",session="idle"} 1`,
		`agentrepl_pages_loaded{reason="shown",session="parked"} 1`,
		`agentrepl_pages_loaded{reason="slot",session="no_session"} 1`,
	)
}

func TestTheLoadedPagesAreTheHostsFrames(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAllocated)
	pages.Held("gone", pageheld.LoadShown)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"a", "k"}, false)
	none := func(context.Context) (map[ids.WorkspaceID]bool, error) { return nil, nil }
	if err := s.RegisterPages(pages, pagesCensus(), none); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="allocated",session="working"} 1`,
		`agentrepl_pages_loaded{reason="unreported",session="keepalive"} 1`,
	)
	if strings.Contains(body, PagesLoadedName+`{reason="shown"`) {
		t.Fatalf("a page Emacs reports but the host does not hold was served:\n%s", body)
	}
}

func TestNoPageIsServedBeforeTheHostsFirstCensus(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAllocated)
	none := func(context.Context) (map[ids.WorkspaceID]bool, error) { return nil, nil }
	if err := s.RegisterPages(pages, pagesCensus(), none); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, PagesLoadedName+"{") {
		t.Fatalf("a page was served before the host said what it holds:\n%s", body)
	}
}

func TestPagesWhoseSessionsCannotBeReadAreOmittedAndSaidOnce(t *testing.T) {
	// Arrange
	s, logs := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("p", pageheld.LoadShown)
	pages.Held("a", pageheld.LoadAllocated)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"p", "a"}, false)
	failing := func(context.Context) (map[ids.WorkspaceID]bool, error) { return nil, errors.New("state closed") }
	if err := s.RegisterPages(pages, pagesCensus(), failing); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	scrape(t, s)
	body := scrape(t, s)

	// Assert
	mustContain(t, body, `agentrepl_pages_loaded{reason="allocated",session="working"} 1`)
	if strings.Contains(body, `session="parked"`) || strings.Contains(body, `session="no_session"`) {
		t.Fatalf("an unread page was served:\n%s", body)
	}
	var errs []map[string]any
	for _, rec := range logs.Records() {
		if rec.Level == "error" && strings.HasPrefix(rec.Message, "the loaded pages' session records could not be read") {
			errs = append(errs, rec.Context)
		}
	}
	if len(errs) != 1 {
		t.Fatalf("errors recorded = %d, want one for the one cause", len(errs))
	}
	if got := errs[0]["error"]; got != "read the session records of 1 loaded pages: state closed" {
		t.Fatalf("error = %v, want the read's cause and the pages it omits", got)
	}
	if got := errs[0]["pages_omitted"]; got != 1 {
		t.Fatalf("pages_omitted = %v (%T), want 1", got, got)
	}
}

func TestAScrapeReadsTheSessionRecordsOnceHoweverManyPagesItServes(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	served := []ids.WorkspaceID{"a", "p1", "p2", "p3", "n1", "n2"}
	for _, ws := range served {
		pages.Held(ws, pageheld.LoadShown)
	}
	pages.HostAttached("host")
	pages.HostReported("host", served, false)
	var reads int
	recorded := func(context.Context) (map[ids.WorkspaceID]bool, error) {
		reads++
		return map[ids.WorkspaceID]bool{"p1": true, "p2": true, "p3": true}, nil
	}
	if err := s.RegisterPages(pages, pagesCensus(), recorded); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	if reads != 1 {
		t.Fatalf("session record reads = %d, want one per scrape", reads)
	}
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="shown",session="working"} 1`,
		`agentrepl_pages_loaded{reason="shown",session="parked"} 3`,
		`agentrepl_pages_loaded{reason="shown",session="no_session"} 2`,
	)
}

func TestAScrapeWhosePagesAllHoldAShimReadsNoSessionRecord(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)
	pages.Held("a", pageheld.LoadAllocated)
	pages.Held("k", pageheld.LoadSlot)
	pages.HostAttached("host")
	pages.HostReported("host", []ids.WorkspaceID{"a", "k"}, false)
	var reads int
	recorded := func(context.Context) (map[ids.WorkspaceID]bool, error) {
		reads++
		return nil, errors.New("not to be read")
	}
	if err := s.RegisterPages(pages, pagesCensus(), recorded); err != nil {
		t.Fatalf("RegisterPages: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	if reads != 0 {
		t.Fatalf("session record reads = %d, want none when every page's workspace holds a shim", reads)
	}
	mustContain(t, body,
		`agentrepl_pages_loaded{reason="allocated",session="working"} 1`,
		`agentrepl_pages_loaded{reason="slot",session="keepalive"} 1`,
	)
}

func TestPageLoadsAndUnloadsAreCountedFromZero(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	pages := pageheld.New(s)

	// Act
	pages.Loaded("a", pageheld.LoadShown)
	pages.Unloaded("a", pageheld.UnloadDeallocated)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_page_loads_total{reason="shown"} 1`,
		`agentrepl_page_loads_total{reason="slot"} 0`,
		`agentrepl_page_unloads_total{reason="deallocated"} 1`,
		`agentrepl_page_unloads_total{reason="released"} 0`,
	)
}

func TestAnUndeclaredUnloadReasonPanics(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an undeclared unload reason did not panic")
		}
	}()

	// Act
	s.CountPageUnload("whim")
}
